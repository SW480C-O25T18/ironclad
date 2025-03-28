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
with Arch.Context;
with Arch.PLIC;
with Arch.CLINT;
with Arch.Snippets;        use Arch.Snippets;
with Arch.DTB;
with Arch.Debug;
with Arch.Local;
with Lib.Panic;
with Scheduler;

package body Arch.Interrupts with SPARK_Mode => Off is

   package FP_Conv is new System.Address_To_Access_Conversions (Arch.Context.FP_Context);

   type IRQ_Table_Type is array (0 .. Max_IRQs) of IRQ_Handler;
   IRQ_Table : IRQ_Table_Type := (others => null);

   IRQ_Counts : array (0 .. Max_IRQs) of Natural := (others => 0);

   procedure Initialize is
   begin
      if not Arch.CLINT.Is_Enabled and then not Arch.PLIC.Is_Enabled then
         Arch.Debug.Print("WARNING: No interrupt controller (PLIC or CLINT) detected. Entering fallback mode.");
      elsif Arch.CLINT.Is_Enabled and then Arch.PLIC.Is_Enabled then
         Arch.Debug.Print("NOTICE: Both CLINT and PLIC are enabled.");
      else
         if Arch.CLINT.Is_Enabled then
            Arch.Debug.Print("CLINT is enabled.");
         end if;
         if Arch.PLIC.Is_Enabled then
            Arch.Debug.Print("PLIC is enabled.");
         end if;
      end if;
   end Initialize;

   function Extract_Priv_Level (SStatus : Unsigned_64) return String is
      SPP : constant Unsigned_64 := SStatus and SSTATUS_SPP;
   begin
      if (SStatus and 16#100#) /= 0 then return "Supervisor";
      elsif (SStatus and 16#300#) = 0 then return "User";
      else return "Machine";
      end if;
   end Extract_Priv_Level;

   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
   begin
      if IRQ < 0 or else IRQ > Max_IRQs then
         raise Constraint_Error with "Invalid IRQ number";
      end if;
      IRQ_Table(IRQ) := Handler;
      Arch.Debug.Print("Register_IRQ: IRQ" & Integer'Image(IRQ));
   end Register_IRQ;

   procedure Unregister_IRQ (IRQ : Integer) is
   begin
      if IRQ < 0 or else IRQ > Max_IRQs then
         raise Constraint_Error with "Invalid IRQ number";
      end if;
      IRQ_Table(IRQ) := null;
      Arch.Debug.Print("Unregister_IRQ: IRQ" & Integer'Image(IRQ));
   end Unregister_IRQ;

   procedure Register_Device_IRQ (Node : access Arch.DTB.DTB_Node; Handler : IRQ_Handler) is
      IRQ : Integer := Arch.DTB.Get_IRQ(Node);
   begin
      if IRQ /= -1 then
         Register_IRQ(IRQ, Handler);
         Arch.Debug.Print("Register_Device_IRQ: IRQ" & Integer'Image(IRQ));
      else
         Arch.Debug.Print("Register_Device_IRQ: No IRQ for " & To_String(Node.Name));
      end if;
   end Register_Device_IRQ;

   procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
      Priv      : constant String := Extract_Priv_Level(Frame_Ptr.sstatus);
      Cause     : constant Unsigned_64 := Frame_Ptr.scause;
      Hart_ID   : constant Unsigned_64 := 0;
      Context_ID: constant Unsigned_64 := 0;
   begin
      case Cause is
         when 5 => Scheduler_Handler (Frame_Ptr);
         when 9 => Syscall_Handler (Frame_Ptr);
         when 3 => Arch.CLINT.Clear_Software_Interrupt (Hart_ID); Scheduler_Handler (Frame_Ptr);
         when 7 => Scheduler_Handler (Frame_Ptr);
         when 11 =>
            declare
               IRQ : Integer := Integer(Arch.PLIC.Claim(Hart_ID, Context_ID));
            begin
               if IRQ >= 0 and then IRQ <= Max_IRQs then
                  IRQ_Counts(IRQ) := IRQ_Counts(IRQ) + 1;
                  Arch.Debug.Print("Dispatching IRQ " & Integer'Image(IRQ) & " Count=" & Natural'Image(IRQ_Counts(IRQ)));
                  if IRQ_Table(IRQ) /= null then
                     IRQ_Table(IRQ).all;
                  else
                     Arch.Debug.Print("No handler registered for IRQ " & Integer'Image(IRQ));
                  end if;
               else
                  Arch.Debug.Print("Invalid IRQ value " & Integer'Image(IRQ));
               end if;
               Arch.PLIC.Complete(Hart_ID, Context_ID, Unsigned_64(IRQ));
            end;
         when others =>
            Lib.Panic.Hard_Panic("Unhandled interrupt: " & Unsigned_64'Image(Cause), Frame_Ptr);
      end case;
   end Handle_Interrupt;

   procedure Save_FP_Context (Frame_Ptr : in out Frame) is
   begin
      if Frame_Ptr.FP_Context_Ptr = System.Null_Address then
         declare
            New_FP : aliased Arch.Context.FP_Context;
         begin
            Arch.Context.Init_FP_Context (New_FP);
            Frame_Ptr.FP_Context_Ptr := New_FP'Address;
         end;
      end if;

      declare
         Access_FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
      begin
         Arch.Context.Save_FP_Context (Access_FP.all);
      end;
   end Save_FP_Context;

   procedure Restore_FP_Context (Frame_Ptr : in out Frame) is
   begin
      if Frame_Ptr.FP_Context_Ptr /= System.Null_Address then
         declare
            Access_FP : FP_Conv.Object_Pointer := FP_Conv.To_Pointer(Frame_Ptr.FP_Context_Ptr);
         begin
            Arch.Context.Load_FP_Context (Access_FP.all);
         end;
      end if;
   end Restore_FP_Context;

   procedure Syscall_Handler (State : in out Frame) is
      Thread_ID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if Thread_ID = Scheduler.Error_TID then
         Arch.Debug.Print("Syscall_Handler: Error_TID encountered!");
         Lib.Panic.Hard_Panic("Syscall invoked with Error_TID", State);
         return;
      end if;

      Arch.Debug.Print("Syscall_Handler: Thread ID = " & Natural'Image(Scheduler.Convert(Thread_ID)));
      Scheduler.Signal_Kernel_Entry (Thread_ID);
      State.x10_a0 := State.x17_a7;
      State.x11_a1 := 0;
      Scheduler.Signal_Kernel_Exit (Thread_ID);
   end Syscall_Handler;

   procedure Scheduler_Handler (State : in out Frame) is
      Thread_ID : constant Scheduler.TID := Arch.Local.Get_Current_Thread;
   begin
      if Thread_ID = Scheduler.Error_TID then
         Arch.Debug.Print("Scheduler_Handler: Error_TID encountered!");
         Lib.Panic.Hard_Panic("Scheduler invoked with Error_TID", State);
         return;
      end if;

      Arch.Debug.Print("Scheduler_Handler: Thread ID = " & Natural'Image(Scheduler.Convert(Thread_ID)));
      Scheduler.Scheduler_ISR (State);
   end Scheduler_Handler;

end Arch.Interrupts;