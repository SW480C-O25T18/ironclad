--  --  arch-exceptions.adb: Package body of interrupt utilities.
--  --  Copyright (C) 2025 Sean C. Weeks - badrock1983
--  --
--  --  This program is free software: you can redistribute it and/or modify
--  --  it under the terms of the GNU General Public License as published by
--  --  the Free Software Foundation, either version 3 of the License, or
--  --  (at your option) any later version.
--  --
--  --  This program is distributed in the hope that it will be useful,
--  --  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  --  GNU General Public License for more details.
--  --
--  --  You should have received a copy of the GNU General Public License
--  --  along with this program.  If not, see <http://www.gnu.org/licenses/>.

--  with Interfaces;            use Interfaces;
--  with System;
--  with System.Address_To_Access_Conversions;
--  with Arch.Context;
--  with Arch.PLIC;
--  with Arch.CLINT;
--  with Arch.Snippets;        use Arch.Snippets;
--  with Arch.DTB;
--  with Arch.Debug;
--  with Arch.Local;
--  with Lib.Panic;
--  with Scheduler;
--  with Userland.Syscall;     use Userland.Syscall;


--  package body Arch.Interrupts with SPARK_Mode => Off is

--     package FP_Conv is new System.Address_To_Access_Conversions (Arch.Context.FP_Context);

--     type IRQ_Table_Type is array (0 .. Max_IRQs) of IRQ_Handler;
--     IRQ_Table : IRQ_Table_Type := (others => null);
--     IRQ_Counts : array (0 .. Max_IRQs) of Natural := (others => 0);

--     procedure Initialize is
--     begin
--        CLINT_Enabled := Arch.CLINT.CLINT_Enabled;
--        PLIC_Enabled  := Arch.PLIC.Is_Enabled;

--        if not CLINT_Enabled and not PLIC_Enabled then
--           Arch.Debug.Print("WARNING: No CLINT or PLIC present — fallback mode enabled.");
--        elsif CLINT_Enabled and PLIC_Enabled then
--           Arch.Debug.Print("CLINT and PLIC both enabled.");
--        elsif CLINT_Enabled then
--           Arch.Debug.Print("CLINT detected and enabled.");
--        elsif PLIC_Enabled then
--           Arch.Debug.Print("PLIC detected and enabled.");
--        end if;
--     end Initialize;

--     procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
--     begin
--        if IRQ in IRQ_Table'Range then
--           IRQ_Table(IRQ) := Handler;
--           Arch.Debug.Print("IRQ " & Integer'Image(IRQ) & " registered.");
--        else
--           raise Constraint_Error with "Invalid IRQ index";
--        end if;
--     end Register_IRQ;

--     procedure Unregister_IRQ (IRQ : Integer) is
--     begin
--        if IRQ in IRQ_Table'Range then
--           IRQ_Table(IRQ) := null;
--           Arch.Debug.Print("IRQ " & Integer'Image(IRQ) & " unregistered.");
--        else
--           raise Constraint_Error with "Invalid IRQ index";
--        end if;
--     end Unregister_IRQ;

--     procedure Register_Device_IRQ (Node : access Arch.DTB.DTB_Node; Handler : IRQ_Handler) is
--        IRQ : constant Integer := Arch.DTB.Get_IRQ(Node);
--     begin
--        if IRQ /= -1 then
--           Register_IRQ(IRQ, Handler);
--           Arch.Debug.Print("Device IRQ " & Integer'Image(IRQ) & " registered for " & To_String(Node.Name));
--        end if;
--     end Register_Device_IRQ;

--     procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
--        Cause : constant Unsigned_64 := Frame_Ptr.scause;
--        Hart  : constant Unsigned_64 := 0;
--     begin
--        case Cause is
--           when 3 =>
--              if CLINT_Enabled then
--                 Arch.CLINT.Clear_Software_Interrupt(Hart);
--                 Scheduler.Scheduler_ISR(Frame_Ptr);
--              end if;

--           when 5 | 7 =>
--              if CLINT_Enabled then
--                 Arch.CLINT.Clear_Timer_Interrupt(Hart);
--                 Scheduler.Scheduler_ISR(Frame_Ptr);
--              end if;

--           when 9 | 11 =>
--              if PLIC_Enabled then
--                 declare
--                    IRQ : Integer := Integer(Arch.PLIC.Claim(Hart, 0));
--                 begin
--                    if IRQ in IRQ_Table'Range then
--                       IRQ_Counts(IRQ) := IRQ_Counts(IRQ) + 1;
--                       Arch.Debug.Print("Dispatching IRQ " & Integer'Image(IRQ) & " Count=" & Natural'Image(IRQ_Counts(IRQ)));
--                       if IRQ_Table(IRQ) /= null then
--                          IRQ_Table(IRQ).all;
--                       else
--                          Arch.Debug.Print("No handler registered for IRQ " & Integer'Image(IRQ));
--                       end if;
--                       Arch.PLIC.Complete(Hart, 0, Unsigned_64(IRQ));
--                    end if;
--                 end;
--              end if;

--           when 8 | 9 =>
--              Syscall_Handler(Frame_Ptr);

--           when others =>
--              Arch.Debug.Print("Unhandled scause=" & Unsigned_64'Image(Cause));
--              Lib.Panic.Hard_Panic("Unhandled IRQ", Frame_Ptr);
--        end case;
--     end Handle_Interrupt;

--     procedure Handle_Trap (Frame_Ptr : access Frame) is
--     begin
--        Arch.Debug.Print("[trap] scause=" & Unsigned_64'Image(Frame_Ptr.scause));
--        Arch.Debug.Print("[trap] sepc=" & Unsigned_64'Image(Frame_Ptr.sepc));
--        Arch.Debug.Print("[trap] sstatus=" & Unsigned_64'Image(Frame_Ptr.sstatus));
   
--     begin
--        Handle_Interrupt (Frame_Ptr.all);
--     exception
--        when others =>
--           Lib.Panic.Hard_Panic("[trap] Unhandled trap exception occurred", Frame_Ptr.all);
--     end;

--        System.Machine_Code.Asm ("j trap_exit", Volatile => True);
--     end Arch.Interrupts.Handle_Trap;

--     procedure Save_FP_Context (Frame_Ptr : in out Frame) is
--     begin
--        if Frame_Ptr.FP_Context_Ptr = System.Null_Address then
--           return;
--        end if;
--        declare
--           FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
--        begin
--           Arch.Context.Save_FP_Context(FP.all);
--        end;
--     end Save_FP_Context;

--     procedure Restore_FP_Context (Frame_Ptr : in out Frame) is
--     begin
--        if Frame_Ptr.FP_Context_Ptr /= System.Null_Address then
--           declare
--              FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
--           begin
--              Arch.Context.Load_FP_Context(FP.all);
--           end;
--        end if;
--     end Restore_FP_Context;

--     procedure Syscall_Handler (State : in out Frame) is
--        Thread_ID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
--     begin
--        if Thread_ID = Scheduler.Error_TID then
--           Arch.Debug.Print("Syscall_Handler: Error_TID encountered!");
--           Lib.Panic.Hard_Panic("Syscall invoked with Error_TID", State);
--           return;
--        end if;

--        Arch.Debug.Print("Syscall_Handler: Thread ID = " & Natural'Image(Scheduler.Convert(Thread_ID)));
--        Scheduler.Signal_Kernel_Entry (Thread_ID);
--        Dispatch_Syscall(State, State.x10_a0, State.x11_a1);
--        Scheduler.Signal_Kernel_Exit (Thread_ID);
--     end Syscall_Handler;

--     procedure Handle_Trap (Frame_Ptr : access Frame) is
--     begin
--        Handle_Interrupt(Frame_Ptr.all);
--        Restore_FP_Context(Frame_Ptr.all);
--     end Handle_Trap;

--  end Arch.Interrupts;