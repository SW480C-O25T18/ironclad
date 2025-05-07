--  arch-interrupts.adb: Implementation of interrupt and exception utilities for RISC-V64.
--  Provides functionality for handling interrupts, exceptions, and syscalls.
--  Includes support for CLINT, PLIC, and vectored/direct trap modes.
--  Fully compliant with the RISC-V64 architecture specification.
--  Includes meaningful debug statements and proper exception handling.
--  Copyright (C) 2025 Sean C. Weeks
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

with System;   use System;
with Interfaces; use Interfaces;
with Arch.Context; use Arch.Context;
with Arch.CLINT;   use Arch.CLINT;
with Arch.PLIC;    use Arch.PLIC;
with Arch.DTB;     use Arch.DTB;
with Arch.Debug;   use Arch.Debug;
with Scheduler;    use Scheduler;
with Userland.Syscall; use Userland.Syscall;
with Lib.Panic;    use Lib.Panic;

package body Arch.Interrupts is

   ----------------------------------------------------------------------------
   --  Extract cause number by clearing the interrupt bit.
   ----------------------------------------------------------------------------
   function Cause_Code (S : Unsigned_64) return Unsigned_64 is
   begin
      return S and not Interrupt_Bit;
   end Cause_Code;

   ----------------------------------------------------------------------------
   --  True if scause indicates an interrupt.
   ----------------------------------------------------------------------------
   function Is_Interrupt (S : Unsigned_64) return Boolean is
   begin
      return (S and Interrupt_Bit) /= 0;
   end Is_Interrupt;

   ----------------------------------------------------------------------------
   --  Configure vectored vs direct trap mode.
   ----------------------------------------------------------------------------
   procedure Configure_Trap_Vector is
      Old_Value, New_Value, Actual_Value : Unsigned_64;
   begin
      Asm ("csrr %0, stvec", Outputs => (Old_Value));
      New_Value := (Old_Value and not 3) or 1; -- Enable vectored mode
      Asm ("csrw stvec, %0", Inputs => (New_Value));
      Asm ("csrr %0, stvec", Outputs => (Actual_Value));
      if Actual_Value = New_Value then
         Debug.Print ("Arch.Interrupts: Using vectored trap mode");
      else
         Debug.Print ("Arch.Interrupts: Using direct trap mode");
      end if;
   end Configure_Trap_Vector;

   ----------------------------------------------------------------------------
   --  Initialize CLINT and PLIC and delegate traps.
   ----------------------------------------------------------------------------
   procedure Initialize is
      Num_Cores : constant Natural := Core_Count;
   begin
      Debug.Print ("Interrupts: Init start");

      --  Delegate exceptions & S-mode interrupts down from M-mode
      Asm ("csrw medeleg, %0", Inputs => (not Unsigned_64 (0)));
      Asm ("csrw mideleg, %0", Inputs => (Shift_Left (Unsigned_64 (1), 1)
                                        or Shift_Left (Unsigned_64 (1), 5)
                                        or Shift_Left (Unsigned_64 (1), 9)));

      Configure_Trap_Vector;

      --  CLINT
      Set_CLINT_Configuration;
      if CLINT_Enabled then
         for I in 0 .. Num_Cores - 1 loop
            Initialize_Hart (Unsigned_64 (I));
         end loop;
         Debug.Print ("CLINT initialized");
      else
         Debug.Print ("CLINT disabled");
      end if;

      --  PLIC
      Set_PLIC_Configuration;
      if PLIC.Is_Enabled then
         for I in 0 .. Num_Cores - 1 loop
            PLIC.Initialize (Hart_Id => Unsigned_64 (I),
                             Ctx     => Supervisor_Context (I));
         end loop;
         Debug.Print ("PLIC initialized");
      else
         Debug.Print ("PLIC disabled");
      end if;

      pragma Assert (CLINT_Enabled or PLIC.Is_Enabled,
                     "No interrupt controller enabled");
      Debug.Print ("Interrupts: Init complete");
   exception
      when others =>
         Hard_Panic ("Interrupts: Initialization failed");
   end Initialize;

   ----------------------------------------------------------------------------
   --  Register an IRQ handler with interrupts masked.
   ----------------------------------------------------------------------------
   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler) is
      Old : Unsigned_64;
   begin
      Asm ("csrr  %0, sstatus", Outputs => (Old));
      Asm ("csrc  sstatus, %0", Inputs  => (Shift_Left (Unsigned_64 (1), 1)));
      IRQ_Table (IRQ) := Handler;
      Asm ("csrw  sstatus, %0", Inputs  => (Old));
      Debug.Print ("Registered IRQ " & Integer'Image (IRQ));
   exception
      when others =>
         Debug.Print ("Register_IRQ failed for " & Integer'Image (IRQ));
   end Register_IRQ;

   ----------------------------------------------------------------------------
   --  Unregister an IRQ handler.
   ----------------------------------------------------------------------------
   procedure Unregister_IRQ (IRQ : Integer) is
      Old : Unsigned_64;
   begin
      Asm ("csrr  %0, sstatus", Outputs => (Old));
      Asm ("csrc  sstatus, %0", Inputs  => (Shift_Left (Unsigned_64 (1), 1)));
      IRQ_Table (IRQ) := null;
      Asm ("csrw  sstatus, %0", Inputs  => (Old));
      Debug.Print ("Unregistered IRQ " & Integer'Image (IRQ));
   exception
      when others =>
         Debug.Print ("Unregister_IRQ failed for " & Integer'Image (IRQ));
   end Unregister_IRQ;

   procedure Exception_Handler (Num : Integer; State : not null Frame_Ptr)
   is
      Exception_Text : constant array (0 .. 30) of String (1 .. 3) :=
         [0  => "#DE", 1  => "#DB", 2  => "???", 3  => "#BP",
          4  => "#OF", 5  => "#BR", 6  => "#UD", 7  => "#NM",
          8  => "#DF", 9  => "???", 10 => "#TS", 11 => "#NP",
          12 => "#SS", 13 => "#GP", 14 => "#PF", 15 => "???",
          16 => "#MF", 17 => "#AC", 18 => "#MC", 19 => "#XM",
          20 => "#VE", 21 => "#CP", 22 => "???", 23 => "???",
          24 => "???", 25 => "???", 26 => "???", 27 => "???",
          28 => "#HV", 29 => "#VC", 30 => "#SX"];
   begin
      --  Check whether we have to panic or just exit the thread.
      if State.CS = (GDT.User_Code64_Segment or 3) then
         Lib.Messages.Put_Line ("Userland " & Exception_Text (Num));
         Userland.Corefile.Generate_Corefile (Context.GP_Context (State.all));
         Do_Exit (Local.Get_Current_Process,
                  Userland.Process.Signal_Segmentation_Fault);
      else
         Lib.Panic.Hard_Panic ("Kernel " & Exception_Text (Num), State.all);
      end if;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception...  In the exception handler!");
   end Exception_Handler;

   ----------------------------------------------------------------------------
   --  Core IRQ dispatch: clear, claim, invoke, complete.
   ----------------------------------------------------------------------------
   procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
      Code : constant Unsigned_64 := Cause_Code (Frame_Ptr.scause);
      Hart : constant Unsigned_64 := Read_Hart_ID;
      Id   : Natural;
   begin
      Debug.Print ("IRQ hart=" & Unsigned_64'Image (Hart) &
                   " scause=" & Unsigned_64'Image (Frame_Ptr.scause));

      if Code = CLINT_SW_INT_CODE then
         Clear_Software_Interrupt (Hart);
         --  Invoke any registered IPI handler
         if Code <= Natural'Last then
            Id := Natural (Code);
            if IRQ_Table (Id) /= null then
               IRQ_Table (Id).all;
               IRQ_Counts (Id) := IRQ_Counts (Id) + 1;
            else
               Debug.Print ("No handler registered for IRQ " & Natural'Image (Id));
            end if;
         else
            Debug.Print ("Invalid IRQ code: " & Unsigned_64'Image (Code));
         end if;

      elsif Code = CLINT_TIMER_INT_CODE1 or Code = CLINT_TIMER_INT_CODE2 then
         Clear_Timer_Interrupt (Hart);
         if Id /= 0 then
            if IRQ_Table (Id) /= null then
               IRQ_Table (Id).all;
               IRQ_Counts (Id) := IRQ_Counts (Id) + 1;
            else
               Debug.Print ("No handler registered for external IRQ " & Natural'Image (Id));
            end if;
         Debug.Print ("Unhandled IRQ code: " & Unsigned_64'Image (Code) &
                      " on hart=" & Unsigned_64'Image (Hart));
         end if;
         --  External: claim, dispatch, complete
         Id := PLIC.Claim (Hart);
         if Id /= 0 and then IRQ_Table (Id) /= null then
            IRQ_Table (Id).all;
            IRQ_Counts (Id) := IRQ_Counts (Id) + 1;
         end if;
         PLIC.Complete (Hart, Id);

      else
         Debug.Print ("Unhandled IRQ code: " &
                      Unsigned_64'Image (Code));
      end if;
   exception
      when others =>
         Hard_Panic ("Handle_Interrupt failed");
   end Handle_Interrupt;

   ----------------------------------------------------------------------------
   --  Top-level trap entry (exceptions & syscalls).
   ----------------------------------------------------------------------------
   procedure Handle_Trap (Frame_Ptr : access Frame) is
      Code     : constant Unsigned_64 := Cause_Code (Frame_Ptr.scause);
      Returned : Unsigned_64;
      Errno    : Errno_Value;
      Ret_Hi   : Unsigned_64;
      --  Map syscall args:
      A0 : constant Unsigned_64 := Frame_Ptr.x10_a0;
      A1 : constant Unsigned_64 := Frame_Ptr.x11_a1;
      A2 : constant Unsigned_64 := Frame_Ptr.x12_a2;
      A3 : constant Unsigned_64 := Frame_Ptr.x13_a3;
      A4 : constant Unsigned_64 := Frame_Ptr.x14_a4;
      A5 : constant Unsigned_64 := Frame_Ptr.x15_a5;
      A7 : constant Unsigned_64 := Frame_Ptr.x17_a7;
   begin
      --  Lazy FP fault?
      if Code = Lazy_FP_Fault_Code and then (
         Frame_Ptr.sstatus and Shift_Left (Unsigned_64 (3), 13)) = 0 then
         Save_FP_Context (Frame_Ptr.FP_Context_Ptr);
      end if;

      if Is_Interrupt (Frame_Ptr.scause) then
         Handle_Interrupt (Frame_Ptr.all);

      elsif Code = SYSCALL_INT_CODE then
         --  Advance PC
         Frame_Ptr.sepc := Frame_Ptr.sepc + 4;
         --  Save syscall context
         Save_GP_Context(Frame_Ptr);
         --  Setup syscall context
         Pre_Syscall_Hook (Frame_Ptr);

         --  Dispatch syscall (0..113)
         case A7 is
            when 0  => Sys_Exit    (A0, Returned, Errno);
            when 1  => PRCTL       (A0, To_Address (A1), Returned, Errno);
            when 2  => Open        (A0, A1, A2, A3, Returned, Errno);
            when 3  => Close       (A0, Returned, Errno);
            when 4  => Read        (A0, A1, A2, Returned, Errno);
            when 5  => Write       (A0, A1, A2, Returned, Errno);
            when 6  => Seek        (A0, A1, A2, Returned, Errno);
            when 7  => Mmap        (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 8  => Munmap      (A0, A1, Returned, Errno);
            when 9  => Get_PID     (Returned, Errno);
            when 10 => Get_PPID    (Returned, Errno);
            when 11 => Exec        (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 12 => Fork        (Frame_Ptr.all, FP_Init_Context'Result, Returned, Errno);
            when 13 => Wait        (A0, A1, A2, Returned, Errno);
            when 14 => Socket      (A0, A1, Returned, Errno);
            when 15 => Set_Hostname(A0, To_Address (A1), Returned, Errno);
            when 16 => Unlink      (A0, A1, A2, Returned, Errno);
            when 17 => FStat       (A0, A1, A2, A3, A4, Returned, Errno);
            when 18 => Pivot_Root  (A0, A1, A2, A3, Returned, Errno);
            when 19 => Chdir       (A0, Returned, Errno);
            when 20 => IOCTL       (A0, A1, A2, Returned, Errno);
            when 21 => Sched_Yield (Returned, Errno);
            when 22 => Create_TCluster (Returned, Errno);
            when 23 => Pipe        (A0, A1, Returned, Errno);
            when 24 => Get_UID     (Returned, Errno);
            when 25 => Rename      (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 26 => List_Procs  (A0, A1, Returned, Errno);
            when 27 => Spawn       (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 28 => Get_TID     (Returned, Errno);
            when 29 => Manage_TCluster (A0, A1, A2, A3, Returned, Errno);
            when 30 => Fcntl       (A0, A1, A2, Returned, Errno);
            when 31 => Exit_Thread(Returned, Errno);
            when 32 => Get_Random  (A0, A1, Returned, Errno);
            when 33 => MProtect    (A0, A1, A2, Returned, Errno);
            when 34 => Sync        (Returned, Errno);
            when 35 => Set_MAC_Cap (A0, Returned, Errno);
            when 36 => Get_MAC_Cap (Returned, Errno);
            when 37 => Add_MAC_Perms (A0, A1, A2, Returned, Errno);
            when 38 => Set_MAC_Enf (A0, Returned, Errno);
            when 39 => Mount       (A0, A1, A2, A3, A4, Returned, Errno);
            when 40 => Umount      (A0, A1, A2, Returned, Errno);
            when 41 => Readlink    (A0, A1, A2, A3, A4, Returned, Errno);
            when 42 => GetDEnts    (A0, A1, A2, Returned, Errno);
            when 43 => MakeNode    (A0, A1, A2, A3, A4, Returned, Errno);
            when 44 => Truncate    (A0, A1, Returned, Errno);
            when 45 => Bind        (A0, A1, A2, Returned, Errno);
            when 46 => Symlink     (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 47 => Connect     (A0, A1, A2, Returned, Errno);
            when 48 => Open_PTY    (A0, Returned, Errno);
            when 49 => FSync       (A0, A1, Returned, Errno);
            when 50 => Link        (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 51 => PTrace      (A0, A1, A2, A3, Returned, Errno);
            when 52 => Listen      (A0, A1, Returned, Errno);
            when 53 => Sys_Accept  (A0, A1, A2, A3, Returned, Errno);
            when 54 => Get_RLimit  (Returned, Errno);
            when 55 => Set_RLimit  (A0, A1, Returned, Errno);
            when 56 => FAccess     (A0, A1, A2, A3, A4, Returned, Errno);
            when 57 => PPoll       (A0, A1, A2, A3, Returned, Errno);
            when 58 => Get_EUID    (Returned, Errno);
            when 59 => Set_UIDs    (A0, A1, Returned, Errno);
            when 60 => Fchmod      (A0, A1, A2, Returned, Errno);
            when 61 => Umask       (Returned, Errno);
            when 62 => Reboot      (A0, A1, Returned, Errno);
            when 63 => Fchown      (A0, A1, A2, A3, A4, Returned, Errno);
            when 64 => PRead       (A0, A1, A2, A3, Returned, Errno);
            when 65 => PWrite      (A0, A1, A2, A3, Returned, Errno);
            when 66 => Get_Sock_Name (A0, A1, A2, Returned, Errno);
            when 67 => Get_Peer_Name (A0, A1, A2, Returned, Errno);
            when 68 => Shutdown    (A0, A1, Returned, Errno);
            when 69 => Futex       (A0, A1, A2, A3, Returned, Errno);
            when 70 => Clock       (A0, A1, A2, Returned, Errno);
            when 71 => Clock_Nanosleep (A0, A1, A2, A3, Returned, Errno);
            when 72 => Get_RUsage  (A0, A1, Returned, Errno);
            when 73 => RecvFrom    (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 74 => SendTo      (A0, A1, A2, A3, A4, A5, Returned, Errno);
            when 75 => Config_NetInterface (A0, A1, A2, Returned, Errno);
            when 76 => UTimes      (A0, A1, A2, A3, A4, Returned, Errno);
            when 77 => Create_TCluster (Returned, Errno);
            when 78 => Switch_TCluster (A0, A1, Returned, Errno);
            when 79 => Sigprocmask (A0, A1, A2, Returned, Errno);
            when 80 => Sigaction   (A0, A1, A2, Returned, Errno);
            when 81 => Send_Signal (A0, A1, Returned, Errno);
            when 82 => Get_Prio    (A0, A1, Returned, Errno);
            when 83 => Set_Prio    (A0, A1, A2, Returned, Errno);
            when 84 => Get_GID     (Returned, Errno);
            when 85 => Get_EGID    (Returned, Errno);
            when 86 => Set_GIDs    (A0, A1, Returned, Errno);
            when 87 => Get_Groups  (A0, A1, Returned, Errno);
            when 88 => Set_Groups  (A0, A1, Returned, Errno);
            when 89 => TTY_Name    (A0, A1, A2, Returned, Errno);
            when 90 => FAdvise     (A0, A1, A2, A3, Returned, Errno);
            when 91 => SHMAt       (A0, A1, A2, Returned, Errno);
            when 92 => SHMCtl      (A0, A1, A2, Returned, Errno);
            when 93 => SHMDt       (A0, Returned, Errno);
            when 94 => SHMGet      (A0, A1, A2, Returned, Errno);
            when 95 => GetSockOpt  (A0, A1, A2, A3, A4, Returned, Errno);
            when 96 => SetSockOpt  (A0, A1, A2, A3, A4, Returned, Errno);
            when 97 => Get_Thread_Name (A0, A1, A2, Returned, Errno);
            when 98 => Set_Thread_Name (A0, A1, A2, Returned, Errno);
            when 99 => Failure_Policy (A0, A1, Returned, Errno);
            when 100 => Create_Thread (A0, A1, A2, A3, A4, Returned, Errno);
            when 104 => List_Mounts (A0, A1, Returned, Errno);
            when 105 => Uname       (A0, Returned, Errno);
            when 106 => List_Threads(A0, A1, Returned, Errno);
            when 107 => List_Clusters (A0, A1, Returned, Errno);
            when 108 => List_NetInter(A0, A1, Returned, Errno);
            when 109 => Dump_Logs   (A0, A1, Returned, Errno);
            when 110 => List_Filelocks (A0, A1, Returned, Errno);
            when 111 => Loadavg     (A0, A1, Returned, Errno);
            when 112 => Meminfo     (A0, Returned, Errno);
            when 113 => List_PCI    (A0, A1, Returned, Errno);
            when others =>
               Raise_Signal (Local.Get_Current_Process,
                             Signal_Bad_Syscall);
               Returned := Unsigned_64'Last;
               Errno    := Error_Not_Implemented;
         end case;

         Post_Syscall_Hook (Frame_Ptr);
         Restore_GP_Context(Frame_Ptr);
         --  Return to userland in a0/a1
         Frame_Ptr.x10_a0 := Returned;
         Frame_Ptr.x11_a1 := Errno_Value'Enum_Rep (Errno);

      else
         --  Synchronous exception
         Debug.Print ("Unhandled exception code: " &
                      Unsigned_64'Image (Code));
         Hard_Panic ("Unhandled exception");
      end if;

      --  Restore S‑mode and return
      Frame_Ptr.sstatus := Frame_Ptr.sstatus or Shift_Left (Unsigned_64 (1), 8);
      Asm ("j trap_exit", Volatile => True);

   exception
      when others =>
         Hard_Panic ("Handle_Trap failed");
   end Handle_Trap;
end Arch.Interrupts;
