--  arch-interrupts.adb: Implementation of interrupt and exception utilities for RISC-V64.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces;                   use Interfaces;
with Arch.CPU;                     use Arch.CPU;
with Arch.Context;                 use Arch.Context;
with Arch.CLINT;                   use Arch.CLINT;
with Arch.PLIC;                    use Arch.PLIC;
with Arch.DTB;                     use Arch.DTB;
with Arch.Debug;                   use Arch.Debug;
with Arch.Local;                   use Arch.Local;
with Scheduler;                    use Scheduler;
with Userland.Syscall;             use Userland.Syscall;
with Lib.Panic;                    use Lib.Panic;
with System;                       use System;
with Ada.Unchecked_Conversion;     use Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body Arch.Interrupts is

   -- Availability flags (cached after init)
   Has_CLINT    : Boolean := False;
   Has_PLIC     : Boolean := False;
   Use_Vectored : Boolean := False;

   -- IRQ dispatch indices
   SWI_IDX   : constant Integer := 0;
   TIMER_IDX : constant Integer := 1;

   -- Mask for SIE bit in sstatus
   SIE_Mask  : constant Unsigned_64 := Shift_Left(Unsigned_64(1), 1);
   -- Mask for FS field (bits 14:13) in sstatus
   FS_Mask   : constant Unsigned_64 := Shift_Left(Unsigned_64(3), 13);

   -- Delegation masks: delegate synchronous exceptions and S-mode interrupts
   MEDELEG_MASK : constant Unsigned_64 := Unsigned_64(-1);
   MIDELEG_MASK: constant Unsigned_64 :=
     Shift_Left(Unsigned_64(1), 1) or Shift_Left(Unsigned_64(1), 5) or
     Shift_Left(Unsigned_64(1), 9);

   -- FP context conversion
   package FP_Conv is new System.Address_To_Access_Conversions(
     Arch.Context.FP_Context);

   -- Frame -> GP_Context for scheduler
   function To_GP is new Ada.Unchecked_Conversion(
     Frame, Arch.Context.GP_Context);

   ------------------------------------------------------------------------
   -- Decode Helpers
   ------------------------------------------------------------------------
   function Cause_Code(S : Unsigned_64) return Unsigned_64 is
   begin
      return S and not Interrupt_Bit;
   end Cause_Code;
   pragma Inline(Cause_Code);

   function Is_Interrupt(S : Unsigned_64) return Boolean is
   begin
      return (S and Interrupt_Bit) /= 0;
   end Is_Interrupt;
   pragma Inline(Is_Interrupt);

   ------------------------------------------------------------------------
   -- TCB offset helper
   ------------------------------------------------------------------------
   function Get_TCB_OFFSET return Unsigned_64 is
   begin
      return Unsigned_64(TCB_CONTEXT_OFFSET);
   end Get_TCB_OFFSET;
   pragma Inline(Get_TCB_OFFSET);

   ------------------------------------------------------------------------
   -- IRQ count metrics
   ------------------------------------------------------------------------
   function Get_IRQ_Count(IRQ : Integer) return Natural is
   begin
      return IRQ_Counts(IRQ);
   end Get_IRQ_Count;
   pragma Inline(Get_IRQ_Count);

   procedure Reset_IRQ_Count(IRQ : Integer) is
   begin
      IRQ_Counts(IRQ) := 0;
   end Reset_IRQ_Count;
   pragma Inline(Reset_IRQ_Count);

   ------------------------------------------------------------------------
   -- Configure trap vector mode (vectored vs direct)
   ------------------------------------------------------------------------
   procedure Configure_Trap_Vector is
      Old_Stvec, New_Stvec, Actual : Unsigned_64;
   begin
      Asm("csrr %0, stvec", Outputs => (Old_Stvec));
      New_Stvec := (Old_Stvec and Shift_Left(Unsigned_64(-1), 2))
                 or Shift_Left(Unsigned_64(1), 0);
      begin
         Asm("csrw stvec, %0", Inputs => (New_Stvec));
      exception when Ada.Interrupts.Interrupt_Error =>
         Debug.Print("Vectored unsupported; using direct");
         return;
      end;
      Asm("csrr %0, stvec", Outputs => (Actual));
      if Actual = New_Stvec then
         Use_Vectored := True;
         Debug.Print("Using vectored interrupts");
      else
         Asm("csrw stvec, %0", Inputs => (Old_Stvec));
         Debug.Print("Using direct interrupts");
      end if;
   end Configure_Trap_Vector;

   ------------------------------------------------------------------------
   -- System initialization
   ------------------------------------------------------------------------
   procedure Initialize is
      Num_Cores : constant Positive := Core_Count;
   begin
      Debug.Print("Interrupts: Init start");

      -- Delegate traps/exceptions to S-mode
      Asm("csrw medeleg, %0", Inputs => (MEDELEG_MASK));
      Asm("csrw mideleg, %0", Inputs => (MIDELEG_MASK));

      Configure_Trap_Vector;

      Set_CLINT_Configuration;
      Has_CLINT := CLINT_Enabled;
      if Has_CLINT then
         Debug.Print("CLINT enabled");
         for I in 0 .. Num_Cores-1 loop
            Initialize_Hart(I);
         end loop;
      else
         Debug.Print("CLINT disabled");
      end if;

      Set_PLIC_Configuration;
      Has_PLIC := PLIC.Is_Enabled;
      if Has_PLIC then
         Debug.Print("PLIC enabled");
         for I in 0 .. Num_Cores-1 loop
            Initialize(I, Supervisor_Context(I));
         end loop;
      else
         Debug.Print("PLIC disabled");
      end if;

      pragma Assert(Has_CLINT or else Has_PLIC,
                    "No interrupt controller enabled");
      Debug.Print("Interrupts: Init complete");
   end Initialize;

   ------------------------------------------------------------------------
   -- Atomic IRQ registration
   ------------------------------------------------------------------------
   procedure Register_IRQ(IRQ : Integer; Handler : IRQ_Handler) is
      Old  : Unsigned_64;
   begin
      pragma Precondition(IRQ in IRQ_Table'Range and Handler/=null);
      Asm("csrr %0, sstatus", Outputs => (Old));
      Asm("csrc sstatus, %0", Inputs  => (SIE_Mask));
      IRQ_Table(IRQ) := Handler;
      Asm("csrw sstatus, %0", Inputs  => (Old));
      Debug.Print("Registered IRQ " & Integer'Image(IRQ));
   end Register_IRQ;

   ------------------------------------------------------------------------
   -- Atomic IRQ unregistration
   ------------------------------------------------------------------------
   procedure Unregister_IRQ(IRQ : Integer) is
      Old : Unsigned_64;
   begin
      pragma Precondition(IRQ in IRQ_Table'Range);
      Asm("csrr %0, sstatus", Outputs => (Old));
      Asm("csrc sstatus, %0", Inputs  => (SIE_Mask));
      IRQ_Table(IRQ) := null;
      Asm("csrw sstatus, %0", Inputs  => (Old));
      Debug.Print("Unregistered IRQ " & Integer'Image(IRQ));
   end Unregister_IRQ;

   ------------------------------------------------------------------------
   -- Device-tree IRQ hook
   ------------------------------------------------------------------------
   procedure Register_Device_IRQ(
     Node    : access DTB_Node_Access;
     Handler : IRQ_Handler) is
      Num : Integer := Arch.DTB.Get_IRQ(Node);
   begin
      pragma Precondition(Node/=null and Handler/=null);
      if Num>=0 then
         Register_IRQ(Num, Handler);
      end if;
   exception
      when others =>
         Debug.Print("DTB IRQ reg failed for " & Node.Name);
         Lib.Panic.Hard_Panic("DTB IRQ reg");
   end Register_Device_IRQ;

   ------------------------------------------------------------------------
   -- Core interrupt dispatch
   ------------------------------------------------------------------------
   procedure Handle_Interrupt(Frame_Ptr : in out Frame) is
      Sc    : constant Unsigned_64 := Frame_Ptr.scause;
      Code  : constant Unsigned_64 := Cause_Code(Sc);
      Hart  : constant Unsigned_64 := Read_Hart_ID;
      Old   : Unsigned_64;
   begin
      Debug.Print("IRQ hart=" & Unsigned_64'Image(Hart) &
                   " scause=" & Unsigned_64'Image(Sc));
      if Is_Interrupt(Sc) then
         case Code is
            when CLINT_SW_INT_CODE =>
               Clear_Software_Interrupt(Hart);
               IRQ_Counts(SWI_IDX):=IRQ_Counts(SWI_IDX)+1;
               Asm("csrr %0, sstatus",Outputs=>(Old));
               Asm("csrs sstatus, %0",Inputs=>(SIE_Mask));
               Scheduler.Scheduler_ISR(To_GP(Frame_Ptr.all));
               Asm("csrw sstatus, %0",Inputs=>(Old));

            when CLINT_TIMER_INT_CODE1|
                 CLINT_TIMER_INT_CODE2 =>
               Clear_Timer_Interrupt(Hart);
               IRQ_Counts(TIMER_IDX):=IRQ_Counts(TIMER_IDX)+1;
               Asm("csrr %0, sstatus",Outputs=>(Old));
               Asm("csrs sstatus, %0",Inputs=>(SIE_Mask));
               Scheduler.Scheduler_ISR(To_GP(Frame_Ptr.all));
               Asm("csrw sstatus, %0",Inputs=>(Old));

            when PLIC_EXT_INT_CODE1|
                 PLIC_EXT_INT_CODE2 =>
               declare
                  IRQ : IRQ_Id :=
                    Claim(Hart, Supervisor_Context(Hart));
               begin
                  if IRQ/=0 then
                     IRQ_Counts(Integer(IRQ)):=
                       IRQ_Counts(Integer(IRQ))+1;
                     Asm("csrr %0, sstatus",Outputs=>(Old));
                     Asm("csrs sstatus, %0",Inputs=>(SIE_Mask));
                     if IRQ_Table(Integer(IRQ))/=null then
                        IRQ_Table(Integer(IRQ)).all;
                     end if;
                     Complete(Hart,
                              Supervisor_Context(Hart),
                              IRQ);
                     Asm("csrw sstatus, %0",Inputs=>(Old));
                  end if;
               end;

            when others =>
               Debug.Print("Spurious IRQ code=" & Unsigned_64'Image(Code));
         end case;
      end if;
   end Handle_Interrupt;

   ------------------------------------------------------------------------
   -- Trap entry (interrupts, syscalls, exceptions)
   ------------------------------------------------------------------------
   procedure Handle_Trap(Frame_Ptr : access Frame) is
      Sc   : constant Unsigned_64 := Frame_Ptr.scause;
      Code : constant Unsigned_64 := Cause_Code(Sc);
      TID  : Scheduler.TID;
   begin
      -- Lazy FP trap: device-not-available
      if Code = 2 and then (Frame_Ptr.sstatus and FS_Mask) = 0 then
         Debug.Print("Arch.Interrupts: Lazy FP enabled");
         declare
            TCB       : System.Address := Arch.Local.Fetch_TCB;
            FP_Ptr    : System.Address := TCB + TCB_FPCTX;
            FP        : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(FP_Ptr);
            TCB_FPReg : System.Address with Address => TCB + TCB_FPCTX;
         begin
            Arch.Context.Init_FP_Context(FP.all);
            TCB_FPReg := FP_Ptr;
            Arch.Local.Load_TCB(TCB);
            -- set FS=Initial
            Asm("csrs sstatus, %0", Inputs => (Shift_Left(Unsigned_64(1), 13)));
            -- update frame to restore FS on return
            Frame_Ptr.sstatus := Frame_Ptr.sstatus or FS_Mask;
            Asm("j trap_exit", Volatile => True);
         end;
      end if;

      Save_FP_Context(Frame_Ptr.all);
      if Is_Interrupt(Sc) then
         Handle_Interrupt(Frame_Ptr.all);
      elsif Code = SYSCALL_INT_CODE then
         -- syscall path
         TID := Arch.Local.Get_Current_Thread;
         Scheduler.Signal_Kernel_Entry(TID);
         Dispatch_Syscall(Frame_Ptr.all,
                          Frame_Ptr.x10_a0,
                          Frame_Ptr.x11_a1);
         Scheduler.Signal_Kernel_Exit(TID);
         Frame_Ptr.sepc := Frame_Ptr.sepc + 4;
      else
         -- synchronous exception dispatch
         case Code is
            when 0 =>
               Debug.Print("Misaligned instruction at " &
                            Unsigned_64'Image(Frame_Ptr.sepc));
               Lib.Panic.Hard_Panic("Misaligned instruction",
                                     To_GP(Frame_Ptr.all));
            when 2 =>
               Debug.Print("Illegal instruction at " &
                            Unsigned_64'Image(Frame_Ptr.sepc));
               Lib.Panic.Hard_Panic("Illegal instruction",
                                     To_GP(Frame_Ptr.all));
            when 4 | 6 =>
               Debug.Print("Misaligned memory at " &
                            Unsigned_64'Image(Frame_Ptr.stval));
               Lib.Panic.Hard_Panic("Misaligned memory",
                                     To_GP(Frame_Ptr.all));
            when 5 | 7 =>
               Debug.Print("Memory access fault at " &
                            Unsigned_64'Image(Frame_Ptr.stval));
               Lib.Panic.Hard_Panic("Memory access fault",
                                     To_GP(Frame_Ptr.all));
            when others =>
               Debug.Print("Unhandled exception code=" &
                            Unsigned_64'Image(Code));
               Lib.Panic.Hard_Panic("Unhandled exception",
                                     To_GP(Frame_Ptr.all));
         end case;
      end if;
      -- restore user S-mode on return
      Frame_Ptr.sstatus := Frame_Ptr.sstatus or SSTATUS_SPP;
      Asm("j trap_exit", Volatile => True);
   end Handle_Trap;

   ------------------------------------------------------------------------
   -- FP context save/restore
   ------------------------------------------------------------------------
   procedure Save_FP_Context(Frame_Ptr : in out Frame) is
   begin
      if Frame_Ptr.FP_Context_Ptr = System.Null_Address then
         return;
      end if;
      declare
         FP : FP_Conv.Object_Pointer :=
           FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
      begin
         Arch.Context.Save_FP_Context(FP.all);
      end;
   end Save_FP_Context;

   procedure Restore_FP_Context(Frame_Ptr : in out Frame) is
   begin
      if Frame_Ptr.FP_Context_Ptr /= System.Null_Address then
         declare
            FP : FP_Conv.Object_Pointer :=
              FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
         begin
            Arch.Context.Load_FP_Context(FP.all);
         exception when others =>
            Lib.Panic.Hard_Panic("Restore_FP_Context error");
         end;
      end if;
   end Restore_FP_Context;

end Arch.Interrupts;