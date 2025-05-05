--  arch-cpu.ads: CPU management routines.
--  Copyright (C) 2024 streaksu
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

with System;
with Interfaces;
with Scheduler;
with Userland.Process;
with Arch.Limine;

package Arch.CPU with SPARK_Mode => Off is
   pragma Pure;

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   subtype Address is System.Address;

   SSTATUS_SPP : constant U64 := 16#100#;  -- bit 8 in sstatus
   --  Core-local data, that each core holds an own version of.
   type Core_Local;
   type Core_Local_Acc is access all Core_Local;
   type Core_Local is record
      --  Do not move the following members used in assembly code.
      Self            : Core_Local_Acc; --  Here for performance reasons.
      Kernel_Stack    : U64;
      User_Stack      : U64;
      --  End of members not to move.
      Number          : Positive;       --  Core number, 1 based.
      Hart_ID         : U64;    --  LAPIC ID of the core.
      Scratch         : Address;        -- CSR tp storage address
      Current_Thread  : Scheduler.TID;
      Current_Process : Userland.Process.PID;
   end record;
   for Core_Local use record
      Self         at 0 range   0 ..  63;
      Kernel_Stack at 0 range  64 .. 127;
      User_Stack   at 0 range 128 .. 191;
   end record;

   --  Core locals and the number of cores, used as an index for the former.
   type Core_Local_Arr is array (Positive range <>) of aliased Core_Local;
   type Core_Local_Arr_Acc is access Core_Local_Arr;
   Core_Count  : Positive;
   Core_Locals : Core_Local_Arr_Acc;

   --  Init the cores and BSP.
   procedure Init_Cores;
   -- Read the core hart ID of the current core.
   function Read_Hart_ID return U64;
   -- Set the trap vector register to the global trap vector entrty point.
   procedure Set_Trap_Vector;

   --  Get the current hart ID from mhartid CSR
   function Self_Hart return U64;

   --  Get access to this hart's per-core local storage
   function Get_Local return access Core_Local;

   --  Global FPU presence flag and XSAVE area size
   Global_Use_FPU : Boolean;
   FPU_Context_Size : U32;

   --  Read arbitrary CSR
   function Read_CSR (CSR_ID : U32) return U64;

   --  Get the BSP hart identifier
   function Get_BSP_Hart return U64;

private

   procedure Core_Bootstrap (Info : access Limine.RISCV64_SMP_CPU_Info)
      with Convention => C, Export;

   procedure Init_Common (Core_Number : Positive; Hart_ID : U64);

end Arch.CPU;
