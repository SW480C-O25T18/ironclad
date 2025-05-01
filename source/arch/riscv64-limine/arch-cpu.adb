--  arch-cpu.adb: CPU management routines.
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

--  arch-cpu.adb: CPU management routines.
--  Copyright (C) 2024 streaksu

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
      -- Properly initialize the request record
      SMP_Request.Base.ID       := Limine.SMP_ID;
      SMP_Request.Base.Revision := 0;
      SMP_Request.Base.Response := Null_Address;
      SMP_Request.Flags         := 0;

      Debug.Print ("Sending SMP request to Limine");
      if SMP_Request.Base.Response = Null_Address then
         Lib.Panic.Hard_Panic ("Limine SMP request needed");
      end if;

      -- Reinterpret the returned pointer as our response record:
      Response := Addr_To_SMP_Response (SMP_Request.Base.Response);

      Debug.Print ("Got SMP response from Limine");
      Core_Count := Natural (Response.CPU_Count);
      Debug.Print ("Got " & Core_Count'Image & " cores");

      BSP_Hart_ID := Response.BSP_Hart_ID;
      Debug.Print ("BSP Hart ID: " & BSP_Hart_ID'Image);

      -- Allocate per-core locals:
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);

      -- Initialize BSP:
      Init_Common (1, BSP_Hart_ID);

      -- Kick off secondary harts:
      if Core_Count > 1 then
         declare
            CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. Response.CPU_Count)
               with Import, Address => Response.CPUs;
         begin
            for Info of CPUs loop
               if Info.Hart_ID /= BSP_Hart_ID then
                  Info.Extra_Arg := Unsigned_64 (Idx);
                  Info.Addr      := Core_Bootstrap'Address;
                  Idx := Idx + 1;
               end if;
            end loop;
         end;
      end if;
   end Init_Cores;

   -------------------------------------------------------------------
   -- Entry point for each secondary hart.
   -------------------------------------------------------------------
   procedure Core_Bootstrap
     (Info : access Limine.RISCV64_SMP_CPU_Info) is
   begin
      Debug.Print ("Hello from core " & Info.Extra_Arg'Image);
      Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID);
   end Core_Bootstrap;

   -------------------------------------------------------------------
   -- Shared per-core initialization.
   -------------------------------------------------------------------
   procedure Init_Common
     (Core_Number : Positive;
      Hart_ID     : Unsigned_64) is
   begin
      Core_Locals (Core_Number) := (
         Self            => Core_Locals(Core_Number)'Access,
         Kernel_Stack    => 0,
         User_Stack      => 0,
         Number          => Core_Number,
         Hart_ID         => Hart_ID,
         Current_Thread  => Scheduler.Error_TID,
         Current_Process => Userland.Process.Error_PID
      );
   end Init_Common;

   -------------------------------------------------------------------
   -- Install our assembly trap stub into the stvec CSR.
   -------------------------------------------------------------------
   procedure Set_Trap_Vector is
   begin
      Asm ("csrw stvec, %0",
           Inputs   => Unsigned_64'Asm_Input (
                         "r",
                         Addr_To_U64 (trap_entry'Address)
                      ),
           Volatile => True
      );
   end Set_Trap_Vector;

   -------------------------------------------------------------------
   -- Read the hardware hart-ID (mhartid CSR).
   -------------------------------------------------------------------
   function Read_Hart_ID return Unsigned_64 is
      ID : Unsigned_64;
   begin
      Asm ("csrr %0, mhartid",
           Outputs  => Unsigned_64'Asm_Output ("=r", ID),
           Clobber  => "memory",
           Volatile => True
      );
      return ID;
   end Read_Hart_ID;

end Arch.CPU;
