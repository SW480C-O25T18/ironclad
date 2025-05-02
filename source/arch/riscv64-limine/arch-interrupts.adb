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
with Arch.Context;           -- For context switching and TCB access
with Arch.PLIC;
with Arch.CLINT;
with Arch.Snippets;        use Arch.Snippets;
with Arch.DTB;
with Arch.Debug;
with Arch.Local;
with Lib.Panic;
with Scheduler;
with Userland.Syscall;     use Userland.Syscall;
with Ada.Interrupts;       use Ada.Interrupts;

package body Arch.Interrupts with SPARK_Mode => Off is

   ------------------------------------------------------------------------------
   -- Symbolic scause codes for RISCV64 interrupts
   --
   -- These constants represent the cause codes for different interrupt types.
   ------------------------------------------------------------------------------
   CLINT_SW_INT_CODE     : constant Unsigned_64 := 3;
   CLINT_TIMER_INT_CODE1 : constant Unsigned_64 := 5;
   CLINT_TIMER_INT_CODE2 : constant Unsigned_64 := 7;
   SYSCALL_INT_CODE      : constant Unsigned_64 := 8;
   PLIC_EXT_INT_CODE1    : constant Unsigned_64 := 9;
   PLIC_EXT_INT_CODE2    : constant Unsigned_64 := 11;

   ------------------------------------------------------------------------------
   -- TCB_CONTEXT_OFFSET
   --
   -- For RISCV64, the TCB (Thread Control Block) pointer is stored in the tp
   -- register per the ABI. The kernel’s TCB layout must include a 64-bit current
   -- context field at a fixed byte offset. Adjust TCB_CONTEXT_OFFSET to match your
   -- RISCV64 TCB layout.
   ------------------------------------------------------------------------------
   TCB_CONTEXT_OFFSET : constant Natural := 72;  -- Adjust as necessary
   function Get_TCP_OFFSET return Unsigned_64 is
      (To_Address(TCB_CONTEXT_OFFSET));

   ------------------------------------------------------------------------------
   -- Get_Current_Context
   --
   -- Retrieves the current context from the TCB.
   --
   -- For RISCV64, this function obtains the TCB pointer via Arch.Local.Fetch_TCB and
   -- then reads the 64-bit context field at TCB + TCB_CONTEXT_OFFSET. If the pointer is
   -- null, it logs a debug message and returns 0.
   --
   -- Returns:
   --   The current context (as Unsigned_64) for the running hart.
   ------------------------------------------------------------------------------
   function Get_Current_Context return Unsigned_64 is
      TCB : constant System.Address := Arch.Local.Fetch_TCB;
   begin
      declare
         type Core_Context_Ptr is access all Unsigned_64;
         CP : Core_Context_Ptr := Core_Context_Ptr(TCB + To_Address(TCB_CONTEXT_OFFSET));
      begin
         if CP = null then
            Arch.Debug.Print("Get_Current_Context: TCB returned a null context; defaulting to 0");
            return 0;
         else
            return CP.all;
         end if;
      end;
   end Get_Current_Context;

   ------------------------------------------------------------------------------
   -- FP_Conv Package
   --
   -- Provides conversion routines for accessing floating-point context.
   ------------------------------------------------------------------------------
   package FP_Conv is new System.Address_To_Access_Conversions (Arch.Context.FP_Context);

   ------------------------------------------------------------------------------
   -- IRQ Table and IRQ_Counts
   --
   -- IRQ_Table holds the registered IRQ handlers indexed by IRQ number.
   -- IRQ_Counts maintains the count of dispatches for each IRQ.
   ------------------------------------------------------------------------------
   type IRQ_Table_Type is array (0 .. Max_IRQs) of IRQ_Handler;
   IRQ_Table  : IRQ_Table_Type := (others => null);
   IRQ_Counts : array (0 .. Max_IRQs) of Natural := (others => 0);

   ------------------------------------------------------------------------------
   -- Get_Current_Hart_ID
   --
   -- Returns the current hart (core) ID for the running processor.
   --
   -- Returns:
   --   The current hart ID (Unsigned_64) as obtained from Arch.CPU.
   ------------------------------------------------------------------------------
   function Get_Current_Hart_ID return Unsigned_64 is
   begin
      return Arch.CPU.Read_Hart_ID;
   end Get_Current_Hart_ID;

   ------------------------------------------------------------------------------
   -- Initialize
   --
   -- Sets up the interrupt handling environment.
   --
   -- This procedure:
   --   1. Queries the number of harts dynamically.
   --   2. Checks whether the CLINT and PLIC are enabled.
   --   3. Initializes the CLINT and PLIC for each hart using the current context
   --      retrieved by Get_Current_Context.
   --   4. Logs progress via Arch.Debug.Print.
   --
   -- Called during system initialization.
   ------------------------------------------------------------------------------

   Has_CLINT : Boolean := False;
   Has_PLIC  : Boolean := False;

   procedure Initialize is
      Num_Harts : constant Unsigned_64 := Arch.Local.Get_Hart_Count;
   begin
      Has_CLINT := Arch.CLINT.CLINT_Enabled;
      Has_PLIC  := Arch.PLIC.Is_Enabled;

      if Has_CLINT or Has_PLIC then
         if Has_CLINT then
            Arch.CLINT.Set_CLINT_Configuration;
            for Hart_ID in 0 .. Num_Harts - 1 loop
               Arch.CLINT.Initialize_Hart(Hart_ID);
               Arch.Debug.Print("CLINT initialized for hart " & Unsigned_64'Image(Hart_ID));
            end loop;
         end if;

         if Has_PLIC then
            Arch.PLIC.Set_PLIC_Configuration;
            for Hart_ID in 0 .. Num_Harts - 1 loop
               Arch.PLIC.Initialize(Hart_ID, Get_Current_Context);
               Arch.Debug.Print("PLIC initialized for hart " & Unsigned_64'Image(Hart_ID));
            end loop;
            Arch.Debug.Print("PLIC detected and enabled.");
         end if;
         Arch.Debug.Print("Arch.Interrupts: Initialize completed successfully.");
      else
         Arch.Debug.Print("Arch.Interrupts: No interrupts detected, interrupt handling disabled.");
      end if;
      Arch.Debug.Print("Arch.Interrupts: Interrupt handling initialized successfully.");
   end Initialize;

   ------------------------------------------------------------------------------
   -- Register_IRQ
   --
   -- Registers an IRQ handler for a specific IRQ number.
   --
   -- Parameters:
   --   IRQ     : The IRQ number for which to register the handler.
   --   Handler : The IRQ handler procedure to be registered.
   --
   -- Raises:
   --   Constraint_Error if the IRQ number is outside the valid range.
   ------------------------------------------------------------------------------
   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
   begin
      Arch.Debug.Print("Register_IRQ: Registering IRQ " & Integer'Image(IRQ) &
                         " with handler at " & Unsigned_64'Image(Unsigned_64(Handler)));
      if IRQ in IRQ_Table'Range then
         IRQ_Table(IRQ) := Handler;
         Arch.Debug.Print("IRQ " & Integer'Image(IRQ) & " registered successfully.");
      else
         Arch.Debug.Print("Register_IRQ: Invalid IRQ index " & Integer'Image(IRQ));
         raise Constraint_Error with "Register_IRQ: Invalid IRQ index";
      end if;
   end Register_IRQ;

   ------------------------------------------------------------------------------
   -- Unregister_IRQ
   --
   -- Unregisters an IRQ handler for a specific IRQ number.
   --
   -- Parameters:
   --   IRQ : The IRQ number to unregister.
   --
   -- Raises:
   --   Constraint_Error if the IRQ number is outside the valid range.
   ------------------------------------------------------------------------------
   procedure Unregister_IRQ (IRQ : Integer) is
   begin
      Arch.Debug.Print("Unregister_IRQ: Unregistering IRQ " & Integer'Image(IRQ));
      if IRQ in IRQ_Table'Range then
         IRQ_Table(IRQ) := null;
         Arch.Debug.Print("IRQ " & Integer'Image(IRQ) & " unregistered successfully.");
      else
         Arch.Debug.Print("Unregister_IRQ: Invalid IRQ index " & Integer'Image(IRQ));
         raise Constraint_Error with "Unregister_IRQ: Invalid IRQ index";
      end if;
   end Unregister_IRQ;

   ------------------------------------------------------------------------------
   -- Register_Device_IRQ
   --
   -- Registers an IRQ handler for a device based on its device tree node.
   --
   -- Parameters:
   --   Node    : Access to the device's DTB node.
   --   Handler : The IRQ handler procedure to be registered.
   --
   -- This procedure retrieves the IRQ number from the device node and registers
   -- the handler accordingly. If no valid IRQ is found, a debug message is printed.
   -- Exceptions are caught and result in a hard panic.
   ------------------------------------------------------------------------------
   procedure Register_Device_IRQ (Node : access Arch.DTB.DTB_Node; Handler : IRQ_Handler) is
      IRQ : constant Integer := Arch.DTB.Get_IRQ(Node);
   begin
      Arch.Debug.Print("Register_Device_IRQ: Registering IRQ " & Integer'Image(IRQ) &
                         " for device " & To_String(Node.Name));
      if IRQ /= -1 then
         Register_IRQ(IRQ, Handler);
         Arch.Debug.Print("Device IRQ " & Integer'Image(IRQ) & " registered for " & To_String(Node.Name));
      else
         Arch.Debug.Print("Register_Device_IRQ: No valid IRQ found for device " & To_String(Node.Name));
      end if;
   exception
      when Constraint_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Constraint error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Constraint error: " & Node.Name);
      when Program_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Program error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Program error: " & Node.Name);
      when Storage_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Storage error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Storage error: " & Node.Name);
      when Tasking_Error =>
         Arch.Debug.Print("Register_Device_IRQ: Tasking error while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Tasking error: " & Node.Name);
      when others =>
         Arch.Debug.Print("Register_Device_IRQ: Unhandled exception while registering device IRQ.");
         Lib.Panic.Hard_Panic("Register_Device_IRQ: Unhandled exception: " & Node.Name);
   end Register_Device_IRQ;

   ------------------------------------------------------------------------------
   -- Handle_Interrupt
   --
   -- The main interrupt handler.
   --
   -- This procedure is invoked upon an interrupt. It:
   --   1. Retrieves the current hart ID and the scause value.
   --   2. Logs the interrupt context.
   --   3. Dispatches the interrupt to the appropriate handler:
   --        - CLINT interrupts (software and timer) are cleared and then
   --          the Scheduler ISR is invoked.
   --        - PLIC external interrupts are claimed, dispatched via the IRQ table,
   --          and completed.
   --        - Syscall interrupts are forwarded to the Syscall_Handler.
   --
   -- If an unhandled interrupt is encountered, a hard panic is triggered.
   --
   -- Parameters:
   --   Frame_Ptr : The pointer to the trap frame containing the CPU state.
   ------------------------------------------------------------------------------
   procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
      Cause : constant Unsigned_64 := Frame_Ptr.scause;
      Hart  : constant Unsigned_64 := Get_Current_Hart_ID;
   begin
      Arch.Debug.Print("Handle_Interrupt: hart=" & Unsigned_64'Image(Hart) &
                         " scause=" & Unsigned_64'Image(Cause) &
                         " sepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                         " sp=" & Unsigned_64'Image(Frame_Ptr.sp) &
                         " sstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                         " sscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch));
      Arch.Debug.Print("Handle_Interrupt: Starting interrupt handler");

      -- Process CLINT interrupts (software and timer)
      if Has_CLINT and (Cause = CLINT_SW_INT_CODE or Cause = CLINT_TIMER_INT_CODE1 or Cause = CLINT_TIMER_INT_CODE2) then
         case Cause is
            when CLINT_SW_INT_CODE =>
               Arch.Debug.Print("Handle_Interrupt: CLINT software interrupt detected");
               Arch.CLINT.Clear_Software_Interrupt(Hart);
               Scheduler.Scheduler_ISR(Frame_Ptr);
            when CLINT_TIMER_INT_CODE1 | CLINT_TIMER_INT_CODE2 =>
               Arch.Debug.Print("Handle_Interrupt: CLINT timer interrupt detected");
               Arch.CLINT.Clear_Timer_Interrupt(Hart);
               Scheduler.Scheduler_ISR(Frame_Ptr);
            when others =>
               Arch.Debug.Print("Handle_Interrupt: Unhandled CLINT interrupt code: " & Unsigned_64'Image(Cause));
               Lib.Panic.Hard_Panic("Unhandled CLINT IRQ", Frame_Ptr);
         end case;
      end if;
            
      -- Process PLIC external interrupts if enabled
      if Has_PLIC and (Cause = PLIC_EXT_INT_CODE1 or Cause = PLIC_EXT_INT_CODE2) then
         case Cause is
            when PLIC_EXT_INT_CODE1 | PLIC_EXT_INT_CODE2 =>
               Arch.Debug.Print("Handle_Interrupt: External interrupt (PLIC) detected");
               declare
                  IRQ : Integer := Integer(Arch.PLIC.Claim(Hart, Get_Current_Context));
               begin
                  Arch.Debug.Print("Handle_Interrupt: Claimed IRQ " & Integer'Image(IRQ));
                  if IRQ in IRQ_Table'Range then
                     IRQ_Counts(IRQ) := IRQ_Counts(IRQ) + 1;
                     if IRQ_Table(IRQ) /= null then
                        Arch.Debug.Print("Handle_Interrupt: Dispatching IRQ " & Integer'Image(IRQ));
                        if IRQ_Table(IRQ) /= null then
                           IRQ_Table(IRQ).Invokes.all;
                        else
                           Arch.Debug.Print("Handle_Interrupt: No handler registered for IRQ " & Integer'Image(IRQ));
                        end if;
                        Arch.Debug.Print("Handle_Interrupt: IRQ " & Integer'Image(IRQ) & " handler executed");
                     else
                        Arch.Debug.Print("Handle_Interrupt: No handler registered for IRQ " & Integer'Image(IRQ));
                     end if;
                  end if;
                  Arch.PLIC.Complete(Hart, Get_Current_Context, Unsigned_64(IRQ));
                  Arch.Debug.Print("Handle_Interrupt: IRQ " & Integer'Image(IRQ) & " completed");
               end;
         end case;
      end if;

      -- Process syscall interrupts
      if Cause = SYSCALL_INT_CODE then
         Arch.Debug.Print("Handle_Interrupt: Syscall interrupt detected, invoking Syscall_Handler");
         Syscall_Handler(Frame_Ptr);
      elsif Cause /= CLINT_SW_INT_CODE and Cause /= CLINT_TIMER_INT_CODE1 and 
            Cause /= CLINT_TIMER_INT_CODE2 and Cause /= PLIC_EXT_INT_CODE1 and 
            Cause /= PLIC_EXT_INT_CODE2 then
         Arch.Debug.Print("Handle_Interrupt: Unhandled scause=" & Unsigned_64'Image(Cause));
         Lib.Panic.Hard_Panic("Unhandled IRQ", Frame_Ptr);
      end if;
      Arch.Debug.Print("Handle_Interrupt: Interrupt completed successfully");
   end Handle_Interrupt;

   ------------------------------------------------------------------------------
   -- Handle_Trap
   --
   -- Handles traps and exceptions.
   --
   -- This procedure is called when a trap (interrupt or exception) occurs.
   -- It:
   --   1. Retrieves the trap cause and current hart ID.
   --   2. Logs the trap context.
   --   3. Invokes Handle_Interrupt to process the trap.
   --   4. If the trap is a syscall, increments the sepc to return to userland.
   --   5. Sets the SSTATUS to return with the correct privileges and jumps to trap_exit.
   --
   -- Parameters:
   --   Frame_Ptr : Access to the trap frame containing the CPU state.
   --
   -- Raises:
   --   Various exceptions which result in a hard panic.
   ------------------------------------------------------------------------------
   procedure Handle_Trap (Frame_Ptr : access Frame) is
      Cause_Code : constant Unsigned_64 := Frame_Ptr.scause and 2#111111#;
      Hart_ID    : constant Unsigned_64 := Arch.Local.Get_Hart_ID;
      Is_Syscall : constant Boolean := (Cause_Code = SYSCALL_INT_CODE);
   begin
      Arch.Debug.Print("Handle_Trap: hart=" & Unsigned_64'Image(Hart_ID) &
                         " scause=" & Unsigned_64'Image(Frame_Ptr.scause) &
                         " sepc=" & Unsigned_64'Image(Frame_Ptr.sepc) &
                         " sp=" & Unsigned_64'Image(Frame_Ptr.sp) &
                         " sstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus) &
                         " sscratch=" & Unsigned_64'Image(Frame_Ptr.sscratch));

      begin
         Arch.Debug.Print("Handle_Trap: Starting trap handler");
         Arch.Debug.Print("Handle_Trap: Invoking Handle_Interrupt");
         Handle_Interrupt(Frame_Ptr.all);
         Arch.Debug.Print("Handle_Trap: Handle_Interrupt completed successfully");
      exception
         when Constraint_Error =>
            Arch.Debug.Print("Handle_Trap: Constraint error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Constraint error", Frame_Ptr.all);
         when Program_Error =>
            Arch.Debug.Print("Handle_Trap: Program error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Program error", Frame_Ptr.all);
         when Storage_Error =>
            Arch.Debug.Print("Handle_Trap: Storage error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Storage error", Frame_Ptr.all);
         when Tasking_Error =>
            Arch.Debug.Print("Handle_Trap: Tasking error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Tasking error", Frame_Ptr.all);
         when Ada.Interrupts.Interrupt_Error =>
            Arch.Debug.Print("Handle_Trap: Interrupt error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Interrupt error", Frame_Ptr.all);
         when Ada.Exceptions.Synchronous_Exception =>
            Arch.Debug.Print("Handle_Trap: Synchronous exception during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Synchronous exception", Frame_Ptr.all);
         when Ada.Exceptions.Asynchronous_Exception =>
            Arch.Debug.Print("Handle_Trap: Asynchronous exception during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Asynchronous exception", Frame_Ptr.all);
         when Ada.Exceptions.Storage_Error =>
            Arch.Debug.Print("Handle_Trap: Storage error during trap handling.");
            Lib.Panic.Hard_Panic("Handle_Trap: Storage error", Frame_Ptr.all);
         when others =>
            Arch.Debug.Print("Handle_Trap: Unhandled exception during trap handling.");
            Lib.Panic.Print_Triple("sepc", "sstatus", "scause",
                                  Frame_Ptr.sepc,
                                  Frame_Ptr.sstatus,
                                  Frame_Ptr.scause);
            Lib.Panic.Hard_Panic("Handle_Trap: Unhandled exception", Frame_Ptr.all);
      end;

      if Is_Syscall then
         Arch.Debug.Print("Handle_Trap: Syscall handler returned, incrementing sepc");
         Frame_Ptr.sepc := Frame_Ptr.sepc + 4;
         Arch.Debug.Print("Handle_Trap: New sepc=" & Unsigned_64'Image(Frame_Ptr.sepc));
      end if;

      Frame_Ptr.sstatus := Frame_Ptr.sstatus or Arch.CPU.SSTATUS_SPP;
      Arch.Debug.Print("Handle_Trap: Returning to sepc=" & Unsigned_64'Image(Frame_Ptr.sepc));
      System.Machine_Code.Asm("j trap_exit", Volatile => True);
   end Handle_Trap;

   ------------------------------------------------------------------------------
   -- Save_FP_Context
   --
   -- Saves the floating-point context for the current thread.
   --
   -- This procedure checks whether the FP context pointer in the trap frame is valid.
   -- If so, it uses Arch.Context.Save_FP_Context to save the FP state. Debug messages
   -- are printed throughout to trace the operation.
   --
   -- Parameters:
   --   Frame_Ptr : The trap frame pointer containing the FP context pointer.
   ------------------------------------------------------------------------------
   procedure Save_FP_Context (Frame_Ptr : in out Frame) is
   begin
      Arch.Debug.Print("Save_FP_Context: Starting FP context save");
      if Frame_Ptr.FP_Context_Ptr = System.Null_Address then
         Arch.Debug.Print("Save_FP_Context: No FP context to save; skipping.");
         return;
      end if;
      declare
         FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
      begin
         Arch.Debug.Print("Save_FP_Context: Saving FP context at " & Unsigned_64'Image(Frame_Ptr.FP_Context_Ptr));
         Arch.Context.Save_FP_Context(FP.all);
         Arch.Debug.Print("Save_FP_Context: FP context saved successfully");
      end;
   end Save_FP_Context;

   ------------------------------------------------------------------------------
   -- Restore_FP_Context
   --
   -- Restores the floating-point context for the current thread.
   --
   -- This procedure checks whether the FP context pointer in the trap frame is valid.
   -- If so, it uses Arch.Context.Load_FP_Context to restore the FP state. If an error
   -- occurs during restore, a hard panic is triggered.
   --
   -- Parameters:
   --   Frame_Ptr : The trap frame pointer containing the FP context pointer.
   ------------------------------------------------------------------------------
   procedure Restore_FP_Context (Frame_Ptr : in out Frame) is
   begin
      Arch.Debug.Print("Restore_FP_Context: Starting FP context restore");
      if Frame_Ptr.FP_Context_Ptr /= System.Null_Address then
         declare
            FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
         begin
            Arch.Context.Load_FP_Context(FP.all);
            Arch.Debug.Print("Restore_FP_Context: FP context restored successfully");
         exception
            when E : others =>
               Arch.Debug.Print("Restore_FP_Context: Error during FP context restore: " & Exception_Message(E));
               Lib.Panic.Hard_Panic("Restore_FP_Context: Error", Frame_Ptr);
         end;
      end if;
      Arch.Debug.Print("Restore_FP_Context: FP context restore completed");
   end Restore_FP_Context;

   ------------------------------------------------------------------------------
   -- Syscall_Handler
   --
   -- Handles system calls from userland.
   --
   -- This procedure is invoked when a syscall interrupt occurs (scause equals SYSCALL_INT_CODE).
   -- It retrieves the current thread ID, signals kernel entry, dispatches the syscall (using
   -- Dispatch_Syscall), signals kernel exit, and logs all operations. If an error is encountered,
   -- a hard panic is triggered.
   --
   -- Parameters:
   --   State : The trap frame containing the syscall state and arguments.
   ------------------------------------------------------------------------------
   procedure Syscall_Handler (State : in out Frame) is
      Thread_ID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      Arch.Debug.Print("Syscall_Handler: Starting syscall handler for thread " &
                        Natural'Image(Scheduler.Convert(Thread_ID)));
      if Thread_ID = Scheduler.Error_TID then
         Lib.Panic.Hard_Panic("Syscall invoked with Error_TID", State);
         return;
      end if;
      Scheduler.Signal_Kernel_Entry(Thread_ID);
      Dispatch_Syscall(State, State.x10_a0, State.x11_a1);
      Scheduler.Signal_Kernel_Exit(Thread_ID);
      Arch.Debug.Print("Syscall_Handler: Syscall completed; returning to userland");
   end Syscall_Handler;

end Arch.Interrupts;
