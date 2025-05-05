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
--  along with Ironclad.  If not, see <http://www.gnu.org/licenses/>.

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
   type SMP_Response_Ptr is access all Limine.RISCV64_SMP_Response;
   function Addr_To_SMP_Response is new Ada.Unchecked_Conversion (
     Source => System.Address,
     Target => SMP_Response_Ptr
   );

   function Addr_To_U64 is new Ada.Unchecked_Conversion (
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
      Response    : Limine.RISCV64_SMP_Response;
   begin
      Arch.Debug.Print ("Init_Cores: Starting core initialization...");
      SMP_Request.Base.ID       := Limine.SMP_ID;
      SMP_Request.Base.Revision := 0;
      SMP_Request.Base.Response := Null_Address;
      SMP_Request.Flags         := 0;

      if SMP_Request.Base.Response = Null_Address then
         Lib.Panic.Hard_Panic ("Init_Cores: Limine SMP request failed.");
      end if;

      Response := Addr_To_SMP_Response (
                     SMP_Request.Base.Response).all;

      Core_Count := Positive (Response.CPU_Count);
      BSP_Hart_ID := Response.BSP_Hart_ID;

      Core_Locals := new Core_Local_Arr (1 .. Core_Count);
      Init_Common (1, U32 (BSP_Hart_ID), 0);

      if Core_Count > 1 then
         declare
            CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. Response.CPU_Count)
               with Import, Address => Response.CPUs;
         begin
            for Info of CPUs loop
               Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID, 0);
            end loop;
         end;
      end if;
   exception
      when others =>
         Arch.Debug.Print ("Init_Cores: exception encountered.");
         Lib.Panic.Hard_Panic ("Init_Cores exception");
   end Init_Cores;

   -------------------------------------------------------------------
   -- Entry point for each secondary hart.
   -------------------------------------------------------------------
   procedure Core_Bootstrap
     (Info : access Limine.RISCV64_SMP_CPU_Info) is
   begin
      Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID, 0);
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Core_Bootstrap exception");
   end Core_Bootstrap;

   -------------------------------------------------------------------
   -- Shared per-core initialization.
   -------------------------------------------------------------------
   procedure Init_Common
     (Total_Harts : U32;
      BSP_Hart    : U32) is
   begin
      Core_Locals (Positive (Self_Hart) + 1) := (
         Self            => Core_Locals (1)'Access,
         Kernel_Stack    => 0,
         User_Stack      => 0,
         Number          => Positive (Self_Hart) + 1,
         Hart_ID         => U64 (Self_Hart),
         Scratch         => null,
         Current_Thread  => Scheduler.Error_TID,
         Current_Process => Userland.Process.Error_PID
      );
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Init_Common exception");
   end Init_Common;

   -------------------------------------------------------------------
   -- Install our assembly trap stub into the stvec CSR.
   -------------------------------------------------------------------
   procedure Set_Trap_Vector is
   begin
      Asm ("csrw stvec, %0",
           Inputs => Unsigned_64'Asm_Input ("r", Addr_To_U64 (trap_entry'Address)));
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Set_Trap_Vector exception");
   end Set_Trap_Vector;

   -------------------------------------------------------------------
   -- Read the hardware hart-ID (mhartid CSR).
   -------------------------------------------------------------------
   function Read_Hart_ID return U64 is
      ID : U64;
   begin
      Asm ("csrr %0, mhartid",
           Outputs => Unsigned_64'Asm_Output ("=r", ID));
      return ID;
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Read_Hart_ID exception");
   end Read_Hart_ID;

   -------------------------------------------------------------------
   -- Get the current hart ID from mhartid CSR
   -------------------------------------------------------------------
   function Self_Hart return U32 is
   begin
      return U32 (Read_Hart_ID);
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Self_Hart exception");
   end Self_Hart;

   -------------------------------------------------------------------
   -- Get access to this hart's per-core local storage
   -------------------------------------------------------------------
   function Get_Local return access Core_Local is
      Addr : U64;
      Ptr  : System.Address;
   begin
      Asm ("csrr %0, tp",
           Outputs => Unsigned_64'Asm_Output ("=r", Addr));
      Ptr := System.Storage_Elements.Integer_To_Address (Addr);
      return Core_Local_Arr_Acc (Ptr).all'Access;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Get_Local failed: Invalid CSR tp");
         Arch.Debug.Print ("CSR tp=" & U64'Image (Addr));
         Lib.Panic.Hard_Panic ("Get_Local exception");
   end Get_Local;

   -------------------------------------------------------------------
   -- Read arbitrary CSR by ID
   -------------------------------------------------------------------
   function Read_CSR (CSR_ID : U32) return U64 is
      Result : U64;
   begin
      Asm ("csrr %0, %1",
           Outputs => Unsigned_64'Asm_Output ("=r", Result),
           Inputs  => Unsigned_32'Asm_Input ("r", CSR_ID));
      return Result;
   exception
      when Constraint_Error =>
         Arch.Debug.Print (
            "[ERROR] Read_CSR failed: Invalid CSR_ID or access violation");
         Arch.Debug.Print ("CSR_ID=" & U32'Image (CSR_ID));
         Arch.Debug.Print (
            "Returning 16#FFFFFFFFFFFFFFFF# to indicate failure");
         -- 0 is a common return value for many CSRs, so we need to
         --  return a different value to indicate failure.
         -- Return a unique value to indicate failure
         return 16#FFFFFFFFFFFFFFFF#;
      when others =>
         Arch.Debug.Print (
            "[ERROR] Read_CSR encountered an unexpected exception");
         Arch.Debug.Print ("CSR_ID=" & U32'Image (CSR_ID));
         Lib.Panic.Hard_Panic ("Read_CSR unexpected exception");
   end Read_CSR;

   -------------------------------------------------------------------
   -- Get the BSP hart identifier
   -------------------------------------------------------------------
   function Get_BSP_Hart return U32 is
   begin
      return U32 (Core_Locals (1).Hart_ID);
   exception
      when others =>
         Lib.Panic.Hard_Panic ("Get_BSP_Hart exception");
   end Get_BSP_Hart;

end Arch.CPU;
