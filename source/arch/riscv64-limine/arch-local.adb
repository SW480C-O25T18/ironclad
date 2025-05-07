--  arch-local.adb: Implementation of CPU-local storage for RISC‑V64.
--  Copyright (C) 2024 streaksu
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

with Lib.Panic;                 use Lib.Panic;
with Arch.CPU;                  use Arch.CPU;
with Scheduler;                 use Scheduler;
with Userland.Process;          use Userland.Process;
with Arch.Context;              use Arch.Context;
with Arch.CLINT;                use Arch.CLINT;
with Interfaces;                use Interfaces;
with System;                    use System;

package body Arch.Local is

   --  Mask for supervisor interrupt-enable (SIE) bit
   SIE_Mask : constant Unsigned_64 := Shift_Left (Unsigned_64 (1), 1);

   ----------------------------------------------------------------------------
   --  Helper to locate the Core_Local record for this hart.
   ----------------------------------------------------------------------------
   function This_CPU return Core_Local_Acc is
      MyHart : constant Unsigned_64 := Read_Hart_ID;
   begin
      for I in Core_Locals'Range loop
         if Core_Locals (I).Hart_ID = MyHart then
            return Core_Locals (I)'Access;
         end if;
      end loop;
      Hard_Panic ("Arch.Local: No Core_Local for hart " & MyHart'Image);
   end This_CPU;

   ----------------------------------------------------------------------------
   --  Reschedule_In: schedule next reschedule after Microseconds.
   --  NOTE: falls back to ASAP; true relative scheduling would read mtime.
   ----------------------------------------------------------------------------
   procedure Reschedule_In (Microseconds : Natural) is
      -- Read the current hardware timer (mtime or SBI)
      Current_Time : Unsigned_64 := Read_Timer;

      -- Convert μs to timer ticks (frequency is in Hz)
      Ticks_Per_Us : constant Unsigned_64 :=
        Timebase_Frequency / 1_000_000;

      -- Ensure the multiplication happens in 64‑bit space
      Time_Delta : Unsigned_64 := Unsigned_64 (Microseconds) * Ticks_Per_Us;
   begin
      -- Schedule the timer for (now + Time_Delta)
      Set_Timer (Current_Time + Time_Delta);

   exception
      when others =>
         -- On any failure, schedule an immediate timer interrupt
         Debug.Print ("Arch.Local: Reschedule_In failed; forcing ASAP");
         Set_Timer (Current_Time);
   end Reschedule_In;

   ----------------------------------------------------------------------------
   --  Reschedule_ASAP: send a software IPI to self
   ----------------------------------------------------------------------------
   procedure Reschedule_ASAP is
      Hart : constant Unsigned_64 := Read_Hart_ID;
   begin
      Send_Software_Interrupt (Hart);
   exception
      when others =>
         Hard_Panic ("Arch.Local: Reschedule_ASAP failed");
   end Reschedule_ASAP;

   ----------------------------------------------------------------------------
   --  Fetch_TCB: read the current TCB pointer from TP register
   ----------------------------------------------------------------------------
   function Fetch_TCB return System.Address is
      Ptr : Unsigned_64;
   begin
      Asm ("mv %0, tp", Outputs => (Ptr));
      return To_Address (Ptr);
   exception
      when others =>
         Hard_Panic ("Arch.Local: Fetch_TCB failed");
   end Fetch_TCB;

   ----------------------------------------------------------------------------
   --  Load_TCB: write the given TCB pointer into TP register
   ----------------------------------------------------------------------------
   procedure Load_TCB (TCB : System.Address) is
      Ptr : constant Unsigned_64 := Unsigned_64 (TCB);
   begin
      Asm ("mv tp, %0", Inputs => (Ptr));
   exception
      when others =>
         Hard_Panic ("Arch.Local: Load_TCB failed");
   end Load_TCB;

   ----------------------------------------------------------------------------
   --  Set_Stacks: set per‑hart user and kernel stack pointers
   ----------------------------------------------------------------------------
   procedure Set_Stacks
     (Core         : Context.Core_Context;
      Kernel_Stack : System.Address)
   is
      Loc : Core_Local_Acc := This_CPU;
   begin
      Loc.User_Stack   := Unsigned_64 (To_Address (Core));
      Loc.Kernel_Stack := Unsigned_64 (To_Address (Kernel_Stack));
   exception
      when others =>
         Hard_Panic ("Arch.Local: Set_Stacks failed");
   end Set_Stacks;

   ----------------------------------------------------------------------------
   --  Get_Current_Thread: atomic read of current thread ID
   ----------------------------------------------------------------------------
   function Get_Current_Thread return Scheduler.TID is
      Returned   : Scheduler.TID;
      Old_Status : Unsigned_64;
      Loc        : Core_Local_Acc := This_CPU;
   begin
      Asm ("csrr %0, sstatus", Outputs => (Old_Status));
      Asm ("csrci sstatus, 2", Volatile => True);
      Returned := Loc.Current_Thread;
      Asm ("csrw sstatus, %0", Inputs => (Old_Status));
      return Returned;
   exception
      when others =>
         Hard_Panic ("Arch.Local: Get_Current_Thread failed");
   end Get_Current_Thread;

   ----------------------------------------------------------------------------
   --  Get_Current_Process: atomic read of current process ID
   ----------------------------------------------------------------------------
   function Get_Current_Process return Userland.Process.PID is
      Returned   : Userland.Process.PID;
      Old_Status : Unsigned_64;
      Loc        : Core_Local_Acc := This_CPU;
   begin
      Asm ("csrr %0, sstatus", Outputs => (Old_Status));
      Asm ("csrci sstatus, 2", Volatile => True);
      Returned := Loc.Current_Process;
      Asm ("csrw sstatus, %0", Inputs => (Old_Status));
      return Returned;
   exception
      when others =>
         Hard_Panic ("Arch.Local: Get_Current_Process failed");
   end Get_Current_Process;

   ----------------------------------------------------------------------------
   --  Set_Current_Thread: atomic write of current thread ID
   ----------------------------------------------------------------------------
   procedure Set_Current_Thread (Thread : Scheduler.TID) is
      Old_Status : Unsigned_64;
      Loc        : Core_Local_Acc := This_CPU;
   begin
      Asm ("csrr %0, sstatus", Outputs => (Old_Status));
      Asm ("csrci sstatus, 2", Volatile => True);
      Loc.Current_Thread := Thread;
      Asm ("csrw sstatus, %0", Inputs  => (Old_Status));
   exception
      when others =>
         Hard_Panic ("Arch.Local: Set_Current_Thread failed");
   end Set_Current_Thread;

   ----------------------------------------------------------------------------
   --  Set_Current_Process: atomic write of current process ID
   ----------------------------------------------------------------------------
   procedure Set_Current_Process (Proc : Userland.Process.PID) is
      Old_Status : Unsigned_64;
      Loc        : Core_Local_Acc := This_CPU;
   begin
      Asm ("csrr %0, sstatus", Outputs => (Old_Status));
      Asm ("csrci sstatus, 2", Volatile => True);
      Loc.Current_Process := Proc;
      Asm ("csrw sstatus, %0", Inputs  => (Old_Status));
   exception
      when others =>
         Hard_Panic ("Arch.Local: Set_Current_Process failed");
   end Set_Current_Process;

end Arch.Local;
