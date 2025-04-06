--  arch-exceptions.adb: Package body of interrupt utilities.
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

with Interfaces;            use Interfaces;
with System;
with System.Address_To_Access_Conversions;
with Arch.CPU;
with Arch.Interrupts;     use Arch.Interrupts;
with Arch.Context;
with Arch.PLIC;
with Arch.CLINT;
with Arch.Snippets;        use Arch.Snippets;
with Arch.DTB;
with Arch.Debug;
with Arch.Local;
with Lib.Panic;
with Scheduler;
with Userland.Syscall;     use Userland.Syscall;


package body Arch.Interrupts with SPARK_Mode => Off is

   package FP_Conv is new System.Address_To_Access_Conversions (Arch.Context.FP_Context);

   type IRQ_Table_Type is array (0 .. Max_IRQs) of IRQ_Handler;
   IRQ_Table : IRQ_Table_Type := (others => null);
   IRQ_Counts : array (0 .. Max_IRQs) of Natural := (others => 0);

   procedure Initialize is
   begin
      CLINT_Enabled := Arch.CLINT.CLINT_Enabled;
      PLIC_Enabled  := Arch.PLIC.Is_Enabled;

      if not CLINT_Enabled and not PLIC_Enabled then
         Arch.Debug.Print("WARNING: No CLINT or PLIC present — fallback mode enabled.");
      elsif CLINT_Enabled and PLIC_Enabled then
         Arch.Debug.Print("CLINT and PLIC both enabled.");
      elsif CLINT_Enabled then
         Arch.Debug.Print("CLINT detected and enabled.");
      elsif PLIC_Enabled then
         Arch.Debug.Print("PLIC detected and enabled.");
      end if;
   end Initialize;

   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
   begin
      Arch.Debug.Print("Register_IRQ: Starting IRQ registration.");
      Arch.Debug.Print("Register_IRQ: Starting registration of IRQ " & Integer'Image(IRQ));
      Arch.Debug.Print("Register_IRQ: Registering IRQ " & Integer'Image(IRQ) & " with handler at " & Unsigned_64'Image(Unsigned_64(Handler)));
      if IRQ in IRQ_Table'Range then
         IRQ_Table(IRQ) := Handler;
         Arch.Debug.Print("IRQ " & Integer'Image(IRQ) & " registered.");
      else
         raise Constraint_Error with "Register_IRQ: Invalid IRQ index";
      end if;
      Arch.Debug.Print("Register_IRQ: IRQ registered successfully.");
   end Register_IRQ;

   procedure Unregister_IRQ (IRQ : Integer) is
   begin
      Arch.Debug.Print("Unregister_IRQ: Starting Unregistering IRQ");
      Arch.Debug.Print("Unregister_IRQ: Unregistering IRQ " & Integer'Image(IRQ));
      if IRQ in IRQ_Table'Range then
         Arch.Debug.Print("Unregister_IRQ: IRQ " & Integer'Image(IRQ) & " found in table.");
         IRQ_Table(IRQ) := null;
         Arch.Debug.Print("Unregister_IRQ: IRQ " & Integer'Image(IRQ) & " unregistered.");
      else
         raise Constraint_Error with "Invalid IRQ index";
      end if;
      Arch.Debug.Print("Unregister_IRQ: IRQ unregistered successfully.");
   end Unregister_IRQ;

   procedure Register_Device_IRQ (Node : access Arch.DTB.DTB_Node; Handler : IRQ_Handler) is
      IRQ : constant Integer := Arch.DTB.Get_IRQ(Node);
   begin
      Arch.Debug.Print("Register_Device_IRQ: Registering IRQ " & Integer'Image(IRQ) & " for device " & To_String(Node.Name));
      if IRQ /= -1 then
         Arch.Debug.Print("Register_Device_IRQ: Device IRQ " & Integer'Image(IRQ) & " found for " & To_String(Node.Name));
         Register_IRQ(IRQ, Handler);
         Arch.Debug.Print("Device IRQ " & Integer'Image(IRQ) & " registered for " & To_String(Node.Name));
      end if;
      Arch.Debug.Print("Register_Device_IRQ: Device " & To_String(Node.Name) & " registered successfully.");
   exception
      when Constraint_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Constraint error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Constraint error", Node);
      when Program_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Program error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Program error", Node);
      when Storage_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Storage error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Storage error", Node);
      when Tasking_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Tasking error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Tasking error", Node);
      when others =>
         Arch.Debug.Print("Register_Device_IRQ: Unhandled exception while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Unhandled exception", Node);
      Arch.Debug.Print("Register_Device_IRQ: Device registration completed successfully.");
   end Register_Device_IRQ;

   procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
      Cause : constant Unsigned_64 := Frame_Ptr.scause;
      Hart  : constant Unsigned_64 := 0;
   begin
      Arch.Debug.Print("Handle_Interrupt: hart=" & Unsigned_64'Image(Hart) &
                     " \nscause=" & Unsigned_64'Image(Cause) &
                     " \nsepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                     " \nsp=" & Unsigned_64'Image(Frame_Ptr.sp) &
                     " \nsstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                     " \nsscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch));
      Arch.Debug.Print("Handle_Interrupt: Starting interrupt handler");
      case Cause is
         when 3 =>
            Arch.Debug.Print("Handle_Interrupt: Internal interrupt (CLINT)");
            if CLINT_Enabled then
               Arch.Debug.Print("Handle_Interrupt: CLINT interrupt detected");
               Arch.Debug.Print("Handle_Interrupt: Clearing CLINT interrupt");
               Arch.CLINT.Clear_Software_Interrupt(Hart);
               Arch.Debug.Print("Handle_Interrupt: CLINT interrupt cleared");
               Arch.Debug.Print("Handle_Interrupt: Calling Scheduler ISR");
               Scheduler.Scheduler_ISR(Frame_Ptr);
               Arch.Debug.Print("Handle_Interrupt: Scheduler ISR completed");
            end if;

         when 5 | 7 =>
            Arch.Debug.Print("Handle_Interrupt: Internal interrupt (CLINT)");
            if CLINT_Enabled then
               Arch.Debug.Print("Handle_Interrupt: CLINT interrupt detected");
               Arch.Debug.Print("Handle_Interrupt: Clearing CLINT interrupt");
               Arch.Debug.Print("Handle_Interrupt: Clearing timer interrupt");
               Arch.CLINT.Clear_Timer_Interrupt(Hart);
               Arch.Debug.Print("Handle_Interrupt: Timer interrupt cleared");
               Arch.Debug.Print("Handle_Interrupt: Calling Scheduler ISR");
               Scheduler.Scheduler_ISR(Frame_Ptr);
               Arch.Debug.Print("Handle_Interrupt: Scheduler ISR completed");
            end if;

         when 9 | 11 =>
            Arch.Debug.Print("Handle_Interrupt: External interrupt (PLIC)");
            if PLIC_Enabled then
               declare
                  IRQ : Integer := Integer(Arch.PLIC.Claim(Hart, 0));
               begin
                  Arch.Debug.Print("Handle_Interrupt: PLIC interrupt detected, IRQ=" & Integer'Image(IRQ));
                  Arch.Debug.Print("Handle_Interrupt: Claiming IRQ " & Integer'Image(IRQ));
                  Arch.Debug.Print("Handle_Interrupt: IRQ=" & Integer'Image(IRQ) &
                                 " \nsepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                                 " \nsp=" & Unsigned_64'Image(Frame_Ptr.sp) &
                                 " \nsstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                                 " \nsscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch));
                  if IRQ in IRQ_Table'Range then
                     Arch.Debug.Print("Handle_Interrupt: Dispatching IRQ " & Integer'Image(IRQ));
                     IRQ_Counts(IRQ) := IRQ_Counts(IRQ) + 1;
                     Arch.Debug.Print("Dispatching IRQ " & Integer'Image(IRQ) & " Count=" & Natural'Image(IRQ_Counts(IRQ)));
                     if IRQ_Table(IRQ) /= null then
                        Arch.Debug.Print("Calling IRQ handler for IRQ " & Integer'Image(IRQ));
                        Arch.Debug.Print("IRQ handler address: " & Unsigned_64'Image(Unsigned_64(IRQ_Table(IRQ))));
                        IRQ_Table(IRQ).all;
                        Arch.Debug.Print("IRQ handler for IRQ " & Integer'Image(IRQ) & " completed successfully");
                     else
                        Arch.Debug.Print("No handler registered for IRQ " & Integer'Image(IRQ));
                     end if;
                     Arch.Debug.Print("Handle_Interrupt: Completing IRQ " & Integer'Image(IRQ));
                     Arch.PLIC.Complete(Hart, 0, Unsigned_64(IRQ));
                     Arch.Debug.Print("Handle_Interrupt: IRQ " & Integer'Image(IRQ) & " completed successfully");
                  end if;
               end;
            end if;

         when 8 | 9 =>
            Arch.Debug.Print("Handle_Interrupt: Syscall interrupt detected, calling Syscall_Handler");
            Syscall_Handler(Frame_Ptr);
            Arch.Debug.Print("Handle_Interrupt: Syscall_Handler completed successfully");
            Arch.Debug.Print("Handle_Interrupt: Syscall interrupt completed, returning to userland");
         when others =>
            Arch.Debug.Print("Unhandled scause=" & Unsigned_64'Image(Cause));
            Lib.Panic.Hard_Panic("Unhandled IRQ", Frame_Ptr);
      end case;
      Arch.Debug.Print("Handle_Interrupt: Interrupt completed successfully");
   end Handle_Interrupt;

   procedure Handle_Trap (Frame_Ptr : access Frame) is
      Cause_Code : constant Unsigned_64 := Frame_Ptr.scause and 2#111111#;
      Hart_ID    : constant Unsigned_64 := Arch.Local.Get_Hart_ID;
      Is_Syscall : constant Boolean := (Cause_Code = 8);
   begin
      Arch.Debug.Print("Handle_Trap: hart=" & Unsigned_64'Image(Hart_ID) &
                     " \nscause=" & Unsigned_64'Image(Frame_Ptr.scause) &
                     " \nsepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                     " \nsp=" & Unsigned_64'Image(Frame_Ptr.sp) & 
                     " \nsstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                     " \nsscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch));

      begin
         Arch.Debug.Print("Handle_Trap: Starting trap handler");
         Handle_Interrupt (Frame_Ptr.all);
         Arch.Debug.Print("Handle_Trap: Trap handler completed successfully");
      exception
         when Constraint_Error =>
            Arch.Debug.Print("Handle_Trap: Constraint error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Constraint error", Frame_Ptr.all);
         when Program_Error =>
            Arch.Debug.Print("Handle_Trap: Program error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Program error", Frame_Ptr.all);
         when Storage_Error =>
            Arch.Debug.Print("Handle_Trap: Storage error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Storage error", Frame_Ptr.all);
         when Tasking_Error =>
            Arch.Debug.Print("Handle_Trap: Tasking error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Tasking error", Frame_Ptr.all);
         when Ada.Interrupts.Interrupt_Error =>
            Arch.Debug.Print("Handle_Trap: Interrupt error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Interrupt error", Frame_Ptr.all);
         when Ada.Exceptions.Synchronous_Exception =>
            Arch.Debug.Print("Handle_Trap: Synchronous exception occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Synchronous exception", Frame_Ptr.all);
         when Ada.Exceptions.Asynchronous_Exception =>
            Arch.Debug.Print("Handle_Trap: Asynchronous exception occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Asynchronous exception", Frame_Ptr.all);
         when Ada.Exceptions.Storage_Error =>
            Arch.Debug.Print("Handle_Trap: Storage error occurred while handling trap.");
            Lib.Panic.Hard_Panic("Handle_Trap: Storage error", Frame_Ptr.all);
         when others =>
            Lib.Panic.Hard_Panic("Handle_Trap: Unhandled trap exception occurred", Frame_Ptr.all);
      end;

      if Is_Syscall then
         Arch.Debug.Print("Handle_Trap: syscall handler returned, incrementing sepc");
         Arch.Debug.Print("Handle_Trap: sepc=" & Unsigned_64'Image(Frame_Ptr.sepc));
         Frame_Ptr.sepc := Frame_Ptr.sepc + 4;
         Arch.Debug.Print("Handle_Trap: sepc + 4=" & Unsigned_64'Image(Frame_Ptr.sepc));
      end if;

      Arch.Debug.Print ("Handle_Trap: Set sstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                        " \nsp=" & Unsigned_64'Image(Frame_Ptr.sp) &
                        " \nsepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                        " \nsscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch) &
                        " \nsepc=" & Unsigned_64'Image(Frame_Ptr.sepc));
      Frame_Ptr.sstatus := Frame_Ptr.sstatus or Arch.CPU.SSTATUS_SPP;
      Arch.Debug.Print("Handle_Trap: returning to sepc=" & Unsigned_64'Image(Frame_Ptr.sepc));

      System.Machine_Code.Asm ("j trap_exit", Volatile => True);
   end Handle_Trap;

   procedure Save_FP_Context (Frame_Ptr : in out Frame) is
   begin
      Arch.Debug.Print("Save_FP_Context: Starting save FP context");
      if Frame_Ptr.FP_Context_Ptr = System.Null_Address then
         Arch.Debug.Print("Save_FP_Context: No FP context to save, skipping.");
         return;
      end if;
      declare
         FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
      begin
         Arch.Debug.Print("Save_FP_Context: Saving FP context at " & Unsigned_64'Image(Frame_Ptr.FP_Context_Ptr));
         Arch.Debug.Print("Save_FP_Context: FP context pointer = " & Unsigned_64'Image(FP.all));
         Arch.Debug.Print("Save_FP_Context: FP context size = " & Natural'Image(FP.all'Length));
         Arch.Context.Save_FP_Context(FP.all);
         Arch.Debug.Print("Save_FP_Context: FP context saved at " & Unsigned_64'Image(Frame_Ptr.FP_Context_Ptr));
      end;
   end Save_FP_Context;

   procedure Restore_FP_Context (Frame_Ptr : in out Frame) is
   begin
      Arch.Debug.Print("Restore_FP_Context: Starting restore FP context");
      if Frame_Ptr.FP_Context_Ptr /= System.Null_Address then
         declare
            FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
         begin
            Arch.Debug.Print("Restore_FP_Context: Restoring FP context at " & Unsigned_64'Image(Frame_Ptr.FP_Context_Ptr));
            Arch.Debug.Print("Restore_FP_Context: FP context pointer = " & Unsigned_64'Image(FP.all));
            Arch.Context.Load_FP_Context(FP.all);
            Arch.Debug.Print("Restore_FP_Context: FP context restored at " & Unsigned_64'Image(Frame_Ptr.FP_Context_Ptr));
         exception
            when Constraint_Error =>
               Arch.Debug.Print("Restore_FP_Context: Constraint error while restoring FP context.");
               Lib.Panic.Hard_Panic("Restore_FP_Context: Constraint error", Frame_Ptr);
            when others =>
               Arch.Debug.Print("Restore_FP_Context: Error while restoring FP context.");
               Lib.Panic.Hard_Panic("Restore_FP_Context: Error", Frame_Ptr);
         end;
      end if;
      Arch.Debug.Print("Restore_FP_Context: FP context restored successfully.");
   end Restore_FP_Context;

   procedure Syscall_Handler (State : in out Frame) is
      Thread_ID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      Arch.Debug.Print("Syscall_Handler: Starting syscall handler");
      Arch.Debug.Print("Syscall_Handler: Thread ID = " & Natural'Image(Scheduler.Convert(Thread_ID)));
      if Thread_ID = Scheduler.Error_TID then
         Arch.Debug.Print("Syscall_Handler: Error_TID encountered!");
         Lib.Panic.Hard_Panic("Syscall invoked with Error_TID", State);
         return;
      end if;

      Arch.Debug.Print("Syscall_Handler: Thread ID = " & Natural'Image(Scheduler.Convert(Thread_ID)));
      Scheduler.Signal_Kernel_Entry (Thread_ID);
      Dispatch_Syscall(State, State.x10_a0, State.x11_a1);
      Scheduler.Signal_Kernel_Exit (Thread_ID);
      Arch.Debug.Print("Syscall_Handler: Syscall completed, returning to userland");
      Arch.Debug.Print("Syscall_Handler: sepc=" & Unsigned_64'Image(State.sepc) &
                        " \nsp=" & Unsigned_64'Image(State.sp));
   end Syscall_Handler;

end Arch.Interrupts;