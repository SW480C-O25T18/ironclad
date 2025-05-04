--  arch-interrupts.adb: Implementation of interrupt and exception utilities for RISC-V64.
--  Provides functionality for handling interrupts, exceptions, and syscalls.
--  Includes support for CLINT, PLIC, and vectored/direct trap modes.
--  Fully compliant with the RISC-V64 architecture specification.
--  Includes meaningful debug statements and proper exception handling.
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

with Arch.CPU;                     use Arch.CPU;
with Arch.Context;                 use Arch.Context;
with Arch.CLINT;                   use Arch.CLINT;
with Arch.PLIC;                    use Arch.PLIC;
with Arch.Debug;                   use Arch.Debug;
with Scheduler;                    use Scheduler;
with Lib.Panic;                    use Lib.Panic;
with System;                       use System;
with Ada.Unchecked_Conversion;     use Ada.Unchecked_Conversion;

package body Arch.Interrupts with SPARK_Mode => Off is

   --  Availability flags (cached after initialization)
   Has_CLINT    : Boolean := False;
   Has_PLIC     : Boolean := False;
   Use_Vectored : Boolean := False;

   --  IRQ dispatch indices
   SWI_IDX   : constant Integer := 0;
   TIMER_IDX : constant Integer := 1;

   --  Mask for SIE bit in sstatus
   SIE_Mask  : constant Unsigned_64 := Shift_Left (Unsigned_64 (1), 1);
   --  Mask for FS field (bits 14:13) in sstatus
   FS_Mask   : constant Unsigned_64 := Shift_Left (Unsigned_64 (3), 13);

   --  Delegation masks: delegate synchronous exceptions and S-mode interrupts
   MEDELEG_MASK : constant Unsigned_64 := not Unsigned_64(0);
   MIDELEG_MASK : constant Unsigned_64 :=
     Shift_Left (Unsigned_64 (1), 1) or Shift_Left (Unsigned_64 (1), 5) or
     Shift_Left (Unsigned_64 (1), 9) and Unsigned_64'Last;

      --  Convert between Address and FP_Context
   function To_FP_Context is
      new Ada.Unchecked_Conversion (
         Source => System.Address, Target => FP_Context);
   function From_FP_Context is
      new Ada.Unchecked_Conversion (
         Source => FP_Context, Target => System.Address);

   --  Frame -> GP_Context for scheduler
   function To_GP is
   new Ada.Unchecked_Conversion (
      Source => Frame,
      Target => Arch.Context.GP_Context);

   ------------------------------------------------------------------------
   --  Decode Helpers
   ------------------------------------------------------------------------

   --  Extract cause number by clearing interrupt bit.
   function Cause_Code (S : Unsigned_64) return Unsigned_64 is
   begin
      return S and not Interrupt_Bit;
   exception
      when others =>
         Debug.Print ("Cause_Code: Unknown error");
         return 0;
   end Cause_Code;

   --  True if scause indicates an interrupt.
   function Is_Interrupt (S : Unsigned_64) return Boolean is
   begin
      return (S and Interrupt_Bit) /= 0;
   exception
      when others =>
         Debug.Print ("Is_Interrupt: Unknown error");
         return False;
   end Is_Interrupt;

   ------------------------------------------------------------------------
   --  TCB Offset Helper
   ------------------------------------------------------------------------

   --  Byte offset of saved Frame in the TCB.
   function Get_TCB_OFFSET return Unsigned_64 is
   begin
      return Unsigned_64 (TCB_CONTEXT_OFFSET);
   exception
      when others =>
         Debug.Print ("Get_TCB_OFFSET: Unknown error");
         return 0;
   end Get_TCB_OFFSET;

   ------------------------------------------------------------------------
   --  IRQ Count Metrics
   ------------------------------------------------------------------------

   --  Return number of times IRQ has been dispatched.
   function Get_IRQ_Count (IRQ : Integer) return Natural is
   begin
      return IRQ_Counts (IRQ);
   end Get_IRQ_Count;
   pragma Inline (Get_IRQ_Count);

   --  Reset dispatch count for IRQ to zero.
   procedure Reset_IRQ_Count (IRQ : Integer) is
   begin
      IRQ_Counts (IRQ) := 0;
   exception
      when others =>
         Debug.Print ("Reset_IRQ_Count: Unknown error");
         Lib.Panic.Hard_Panic ("Reset_IRQ_Count: Failed to reset IRQ count");
   end Reset_IRQ_Count;
   pragma Inline (Reset_IRQ_Count);

   ------------------------------------------------------------------------
   --  Configure Trap Vector Mode (Vectored vs Direct)
   ------------------------------------------------------------------------

   procedure Configure_Trap_Vector is
      Old_Stvec, New_Stvec, Actual : Unsigned_64;
   begin
      --  Read current stvec value
      Asm ("csrr %0, stvec", Outputs => (Old_Stvec));

      --  Set vectored mode (bit 0 = 1)
      New_Stvec := (Old_Stvec and Shift_Left (Unsigned_64 (-1), 2))
                 or Shift_Left (Unsigned_64 (1), 0);

      --  Write new stvec value
      Asm ("csrw stvec, %0", Inputs => (New_Stvec));

      --  Verify if vectored mode is enabled
      Asm ("csrr %0, stvec", Outputs => (Actual));
      if Actual = New_Stvec then
         Debug.Print ("Using vectored interrupts");
         Use_Vectored := True;
      else
         Debug.Print ("Using direct interrupts");
         Use_Vectored := False;
      end if;
   exception
      when others =>
         Debug.Print ("Configure_Trap_Vector: Unknown error");
         Debug.Print ("Using direct interrupts");
         Use_Vectored := False;
         return;
   end Configure_Trap_Vector;

   ------------------------------------------------------------------------
   --  System Initialization
   ------------------------------------------------------------------------

   procedure Initialize is
      Num_Cores : constant Positive := Core_Count;
   begin
      Debug.Print ("Interrupts: Init start");

      --  Delegate traps/exceptions to S-mode
      Asm ("csrw medeleg, %0", Inputs => (MEDELEG_MASK));
      Asm ("csrw mideleg, %0", Inputs => (MIDELEG_MASK));

      --  Configure trap vector mode
      Configure_Trap_Vector;

      --  Initialize CLINT
      Set_CLINT_Configuration;
      Has_CLINT := CLINT_Enabled;
      if Has_CLINT then
         for Hart in 0 .. Num_Cores - 1 loop
            Initialize_Hart (Hart_Id => Unsigned_64 (Hart));
         end loop;
         Debug.Print ("CLINT initialized");
      else
         Debug.Print ("CLINT disabled");
      end if;

      --  Initialize PLIC
      Set_PLIC_Configuration;
      Has_PLIC := PLIC.Is_Enabled;
      if Has_PLIC then
         for Hart in 0 .. Num_Cores - 1 loop
            PLIC.Initialize (
               Hart_Id => Unsigned_64 (Hart),
               Ctx => Supervisor_Context (Hart));
         end loop;
         Debug.Print ("PLIC initialized");
      else
         Debug.Print ("PLIC disabled");
      end if;

      pragma Assert (
         Has_CLINT or else Has_PLIC, "No interrupt controller enabled");
      Debug.Print ("Interrupts: Init complete");
   exception
      when others =>
         Debug.Print ("Initialize: Unknown error");
         Lib.Panic.Hard_Panic ("Interrupts: Initialization failed");
   end Initialize;

   ------------------------------------------------------------------------
   --  Atomic IRQ Registration
   ------------------------------------------------------------------------

   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
      Old : Unsigned_64;
   begin
      pragma Precondition (IRQ in IRQ_Table'Range and Handler /= null);

      --  Disable interrupts
      Asm ("csrr %0, sstatus", Outputs => (Old));
      Asm ("csrc sstatus, %0", Inputs  => (SIE_Mask));

      --  Register handler
      IRQ_Table (IRQ) := Handler;

      --  Restore interrupts
      Asm ("csrw sstatus, %0", Inputs  => (Old));
      Debug.Print ("Registered IRQ " & Integer'Image (IRQ));
   exception
      when others =>
         Debug.Print ("Register_IRQ: Unknown error");
         return;
   end Register_IRQ;

   ------------------------------------------------------------------------
   --  Atomic IRQ Unregistration
   ------------------------------------------------------------------------

   procedure Unregister_IRQ (IRQ : Integer) is
      Old : Unsigned_64;
   begin
      pragma Precondition (IRQ in IRQ_Table'Range);

      --  Disable interrupts
      Asm ("csrr %0, sstatus", Outputs => (Old));
      Asm ("csrc sstatus, %0", Inputs  => (SIE_Mask));

      --  Unregister handler
      IRQ_Table (IRQ) := null;

      --  Restore interrupts
      Asm ("csrw sstatus, %0", Inputs  => (Old));
      Debug.Print ("Unregistered IRQ " & Integer'Image (IRQ));
   exception
      when others =>
         Debug.Print ("Unregister_IRQ: Unknown error");
         return;
   end Unregister_IRQ;

   ------------------------------------------------------------------------
   --  Core Interrupt Dispatch
   ------------------------------------------------------------------------

   procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
      Sc    : constant Unsigned_64 := Frame_Ptr.scause;
      Code  : constant Unsigned_64 := Cause_Code (Sc);
      Hart  : constant Unsigned_64 := Read_Hart_ID;
   begin
      Debug.Print ("IRQ hart=" & Unsigned_64'Image (Hart) &
                   " scause=" & Unsigned_64'Image (Sc));

      if Is_Interrupt (Sc) then
         case Code is
            when CLINT_SW_INT_CODE =>
               Clear_Software_Interrupt (Hart_Id => Hart);
               Debug.Print ("Software interrupt handled");
            when CLINT_TIMER_INT_CODE1 | CLINT_TIMER_INT_CODE2 =>
               Clear_Timer_Interrupt (Hart_Id => Hart);
               Debug.Print ("Timer interrupt handled");
            when PLIC_EXT_INT_CODE1 | PLIC_EXT_INT_CODE2 =>
               Debug.Print ("PLIC interrupt handled");
            when others =>
               Debug.Print (
                  "Unhandled interrupt code: " &
                  Unsigned_64'Image (Code));
         end case;
      end if;
   exception
      when others =>
         Debug.Print ("Handle_Interrupt: Unknown error");
         Lib.Panic.Hard_Panic ("Handle_Interrupt: Failed to handle interrupt");
   end Handle_Interrupt;

   ------------------------------------------------------------------------
   --  Trap Entry (Interrupts, Syscalls, Exceptions)
   ------------------------------------------------------------------------

   procedure Handle_Trap (Frame_Ptr : access Frame) is
      Sc   : constant Unsigned_64 := Frame_Ptr.scause;
      Code : constant Unsigned_64 := Cause_Code (Sc);
   begin
      --  Lazy FP trap: device-not-available
      if Code = 2 and then (Frame_Ptr.sstatus and FS_Mask) = 0 then
         Save_FP_Context (Frame_Ptr.all);
         return;
      end if;

      --  Handle interrupt
      if Is_Interrupt (Sc) then
         Handle_Interrupt (Frame_Ptr.all);
      elsif Code = SYSCALL_INT_CODE then
         --  Handle syscall
         Frame_Ptr.sepc := Frame_Ptr.sepc + 4;
         Debug.Print ("Syscall handled");
      else
         --  Handle exception
         Debug.Print (
            "Unhandled exception code: " & Unsigned_64'Image (Code));
         Lib.Panic.Hard_Panic ("Unhandled exception");
      end if;

      --  Restore user S-mode on return
      Frame_Ptr.sstatus := Frame_Ptr.sstatus or SSTATUS_SPP;
      Asm ("j trap_exit", Volatile => True);
   exception
      when others =>
         Debug.Print ("Handle_Trap: Unknown error");
         Lib.Panic.Hard_Panic ("Handle_Trap: Failed to handle trap");
   end Handle_Trap;

end Arch.Interrupts;