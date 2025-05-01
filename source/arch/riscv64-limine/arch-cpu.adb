--  arch-cpu.adb: CPU management routines.
--  Handles CPU initialization, core management, and exception handling.
--  Copyright (C) 2024 streaksu
--  Copyright (C) 2025 scweeks
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

with Arch.Debug;
with System;              use System;
with System.Machine_Code; use System.Machine_Code;
with Lib.Panic;
with Arch.Limine;         use Arch.Limine;
with Ada.Unchecked_Conversion;
with Arch.Interrupts;     use Arch.Interrupts;

package body Arch.CPU with SPARK_Mode => Off is

   -------------------------------------------------------------------
   -- Helpers to reinterpret a raw address as the SMP response record
   -- and to reinterpret any Address as Unsigned_64 for asm inputs.
   -------------------------------------------------------------------
   function Addr_To_SMP_Response is
     new Ada.Unchecked_Conversion (
       Source => System.Address,
       Target => Limine.RISCV64_SMP_Response
     );

   function Addr_To_U64 is
     new Ada.Unchecked_Conversion (
       Source => System.Address,
       Target => Unsigned_64
     );

   -------------------------------------------------------------------
   -- SMP_Request: tell Limine we want SMP info.
   -------------------------------------------------------------------
   SMP_Request : Limine.SMP_Request
      with Export, Async_Writers;

   -------------------------------------------------------------------
   -- Init_Cores: ask Limine for hart count and wake them.
   -------------------------------------------------------------------
   procedure Init_Cores is
      BSP_Hart_ID : Unsigned_64;
      Idx         : Natural := 2;
      Response    : Limine.RISCV64_SMP_Response;
   begin
      Arch.Debug.Print ("Init_Cores: Starting core initialization...");

      -- Properly initialize the request record
      SMP_Request.Base.ID       := Limine.SMP_ID;
      SMP_Request.Base.Revision := 0;
      SMP_Request.Base.Response := Null_Address;
      SMP_Request.Flags         := 0;

      Arch.Debug.Print ("Init_Cores: Sending SMP request to Limine...");
      if SMP_Request.Base.Response = Null_Address then
         Lib.Panic.Hard_Panic ("Init_Cores: Limine SMP request failed.");
      end if;

      -- Reinterpret the returned pointer as our response record:
      Response := Addr_To_SMP_Response (SMP_Request.Base.Response);

      Arch.Debug.Print ("Init_Cores: Received SMP response from Limine.");
      Core_Count := Natural (Response.CPU_Count);
      Arch.Debug.Print ("Init_Cores: Detected " & Core_Count'Image & " cores.");

      BSP_Hart_ID := Response.BSP_Hart_ID;
      Arch.Debug.Print ("Init_Cores: BSP Hart ID is " & BSP_Hart_ID'Image);

      -- Allocate per-core locals:
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);

      -- Initialize BSP:
      Arch.Debug.Print ("Init_Cores: Initializing BSP (Core 1)...");
      Init_Common (1, BSP_Hart_ID);

      -- Kick off secondary harts:
      if Core_Count > 1 then
         declare
            CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. Response.CPU_Count)
               with Import, Address => Response.CPUs;
         begin
            for Info of CPUs loop
               Arch.Debug.Print ("Init_Cores: Initializing core "
                  & Info.Extra_Arg'Image & " with Hart ID "
                  & Info.Hart_ID'Image);
               Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID);
            end loop;
         end;
      end if;

      Arch.Debug.Print ("Init_Cores: All cores initialized successfully.");
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Init_Cores: Constraint_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Cores: Constraint_Error during initialization.");
      when Program_Error =>
         Arch.Debug.Print ("Init_Cores: Program_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Cores: Program_Error during initialization.");
      when Storage_Error =>
         Arch.Debug.Print ("Init_Cores: Storage_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Cores: Storage_Error during initialization.");
      when others =>
         Arch.Debug.Print ("Init_Cores: Unknown exception encountered.");
         Lib.Panic.Hard_Panic ("Init_Cores: Unknown exception during initialization.");
   end Init_Cores;

   -------------------------------------------------------------------
   -- Entry point for each secondary hart.
   -------------------------------------------------------------------
   procedure Core_Bootstrap
     (Info : access Limine.RISCV64_SMP_CPU_Info) is
   begin
      Arch.Debug.Print ("Core_Bootstrap: Core " & Info.Extra_Arg'Image
         & " is booting with Hart ID " & Info.Hart_ID'Image);
      Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID);
      Arch.Debug.Print ("Core_Bootstrap: Core " & Info.Extra_Arg'Image
         & " initialized successfully.");
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Core_Bootstrap: Constraint_Error encountered.");
         Lib.Panic.Hard_Panic ("Core_Bootstrap: Constraint_Error during core bootstrap.");
      when Program_Error =>
         Arch.Debug.Print ("Core_Bootstrap: Program_Error encountered.");
         Lib.Panic.Hard_Panic ("Core_Bootstrap: Program_Error during core bootstrap.");
      when Storage_Error =>
         Arch.Debug.Print ("Core_Bootstrap: Storage_Error encountered.");
         Lib.Panic.Hard_Panic ("Core_Bootstrap: Storage_Error during core bootstrap.");
      when others =>
         Arch.Debug.Print ("Core_Bootstrap: Unknown exception encountered.");
         Lib.Panic.Hard_Panic ("Core_Bootstrap: Unknown exception during core bootstrap.");
   end Core_Bootstrap;

   -------------------------------------------------------------------
   -- Shared per-core initialization.
   -------------------------------------------------------------------
   procedure Init_Common
     (Core_Number : Positive;
      Hart_ID     : Unsigned_64) is
   begin
      Arch.Debug.Print ("Init_Common: Initializing core " & Core_Number'Image
         & " with Hart ID " & Hart_ID'Image);

      Core_Locals (Core_Number) := (
         Self            => Core_Locals(Core_Number)'Access,
         Kernel_Stack    => 0,
         User_Stack      => 0,
         Number          => Core_Number,
         Hart_ID         => Hart_ID,
         Current_Thread  => Scheduler.Error_TID,
         Current_Process => Userland.Process.Error_PID
      );

      Arch.Debug.Print ("Init_Common: Core " & Core_Number'Image
         & " initialized successfully.");
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Init_Common: Constraint_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Common: Constraint_Error during core initialization.");
      when Program_Error =>
         Arch.Debug.Print ("Init_Common: Program_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Common: Program_Error during core initialization.");
      when Storage_Error =>
         Arch.Debug.Print ("Init_Common: Storage_Error encountered.");
         Lib.Panic.Hard_Panic ("Init_Common: Storage_Error during core initialization.");
      when others =>
         Arch.Debug.Print ("Init_Common: Unknown exception encountered.");
         Lib.Panic.Hard_Panic ("Init_Common: Unknown exception during core initialization.");
   end Init_Common;

   -------------------------------------------------------------------
   -- Install our assembly trap stub into the stvec CSR.
   -------------------------------------------------------------------
   procedure Set_Trap_Vector is
   begin
      Arch.Debug.Print ("Set_Trap_Vector: Setting trap vector...");
      Asm ("csrw stvec, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Addr_To_U64 (trap_entry'Address)));
      Arch.Debug.Print ("Set_Trap_Vector: Trap vector set successfully.");
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Set_Trap_Vector: Constraint_Error encountered.");
         Lib.Panic.Hard_Panic ("Set_Trap_Vector: Constraint_Error during trap vector setup.");
      when Program_Error =>
         Arch.Debug.Print ("Set_Trap_Vector: Program_Error encountered.");
         Lib.Panic.Hard_Panic ("Set_Trap_Vector: Program_Error during trap vector setup.");
      when Storage_Error =>
         Arch.Debug.Print ("Set_Trap_Vector: Storage_Error encountered.");
         Lib.Panic.Hard_Panic ("Set_Trap_Vector: Storage_Error during trap vector setup.");
      when others =>
         Arch.Debug.Print ("Set_Trap_Vector: Unknown exception encountered.");
         Lib.Panic.Hard_Panic ("Set_Trap_Vector: Unknown exception during trap vector setup.");
   end Set_Trap_Vector;

   -------------------------------------------------------------------
   -- Read the hardware hart-ID (mhartid CSR).
   -------------------------------------------------------------------
   function Read_Hart_ID return Unsigned_64 is
      ID : Unsigned_64;
   begin
      Arch.Debug.Print ("Read_Hart_ID: Reading Hart ID...");
      Asm ("csrr %0, mhartid",
           Outputs => Unsigned_64'Asm_Output ("=r", ID));
      Arch.Debug.Print ("Read_Hart_ID: Hart ID is " & ID'Image);
      return ID;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Read_Hart_ID: Constraint_Error encountered.");
         Lib.Panic.Hard_Panic ("Read_Hart_ID: Constraint_Error during Hart ID read.");
      when Program_Error =>
         Arch.Debug.Print ("Read_Hart_ID: Program_Error encountered.");
         Lib.Panic.Hard_Panic ("Read_Hart_ID: Program_Error during Hart ID read.");
      when Storage_Error =>
         Arch.Debug.Print ("Read_Hart_ID: Storage_Error encountered.");
         Lib.Panic.Hard_Panic ("Read_Hart_ID: Storage_Error during Hart ID read.");
      when others =>
         Arch.Debug.Print ("Read_Hart_ID: Unknown exception encountered.");
         Lib.Panic.Hard_Panic ("Read_Hart_ID: Unknown exception during Hart ID read.");
   end Read_Hart_ID;

end Arch.CPU;