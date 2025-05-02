--  arch-clint.adb: Core Local Interruptor (CLINT) utilities for RISC-V64.
--  Provides support for software interrupts and timer management.
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

with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with Interfaces;               use Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code;
with Arch.Debug;

package body Arch.CLINT with SPARK_Mode => Off is

   --------------------------------------------------------------------------
   --  CLINT Configuration Record
   --------------------------------------------------------------------------
   type CLINT_Rec is record
      Base_Address    : Address;
      MSIP_Offset     : Unsigned_64;
      MTime_Offset    : Unsigned_64;
      MTimeCmp_Offset : Unsigned_64;
      Enabled         : Boolean;
   end record;

   CLINT_State : CLINT_Rec := (
      Base_Address    => To_Address (16#0200_0000#),
      MSIP_Offset     => 0,
      MTime_Offset    => 16#BFF8#,
      MTimeCmp_Offset => 16#4000#,
      Enabled         => True
   );

   --------------------------------------------------------------------------
   --  Address <=> Unsigned_64 conversion helpers
   --------------------------------------------------------------------------
   function Addr_To_U64 is new Ada.Unchecked_Conversion(
      Source => Address,
      Target => Unsigned_64
   );

   function U64_To_Addr is new Ada.Unchecked_Conversion(
      Source => Unsigned_64,
      Target => Address
   );

   --------------------------------------------------------------------------
   --  Volatile MMIO register access
   --------------------------------------------------------------------------
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr is access all Reg_Type;

   function To_Reg_Ptr is new Ada.Unchecked_Conversion(
      Source => Address,
      Target => Reg_Ptr
   );

   function Reg (Addr : Address) return Reg_Ptr is
   begin
      Arch.Debug.Print (
         "clint.Reg: addr=0x" & Unsigned_64'Image (Addr_To_U64 (Addr))
      );
      return To_Reg_Ptr (Addr);
   end Reg;

   --------------------------------------------------------------------------
   --  Memory barrier (fence)
   --------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      System.Machine_Code.Asm (
         "fence",
         Volatile => True,
         Clobber  => "memory"
      );
      Arch.Debug.Print ("clint.Memory_Barrier executed");
   end Memory_Barrier;

   --------------------------------------------------------------------------
   --  Software Interrupt (MSIP) Management
   --------------------------------------------------------------------------
   procedure Set_Software_Interrupt (
     Hart_ID : Unsigned_64;
     Value   : Boolean
   ) is
      Base_Int : constant Integer_Address :=
        Integer_Address (CLINT_State.Base_Address);
      Raw_Off  : constant Integer_Address :=
         Base_Int
       + Integer_Address (Natural (CLINT_State.MSIP_Offset))
       + Integer_Address (Natural (Hart_ID * 4));
      Addr     : constant Address := To_Address (Raw_Off);
      MSIP_Reg : Reg_Ptr := Reg (Addr);
   begin
      if not CLINT_State.Enabled then
         Arch.Debug.Print ("clint.Set_Software_Interrupt: disabled");
         return;
      end if;
      if Value then
         MSIP_Reg.all := 1;
      else
         MSIP_Reg.all := 0;
      end if;
      Memory_Barrier;
      Arch.Debug.Print (
         "clint.Set_Software_Interrupt: hart=" &
         Unsigned_64'Image (Hart_ID) &
         " value=" & Boolean'Image (Value)
      );
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Set_Software_Interrupt (Hart_ID, False);
   end Clear_Software_Interrupt;

   function Read_Software_Interrupt (
     Hart_ID : Unsigned_64
   ) return Boolean is
      Base_Int : constant Integer_Address :=
        Integer_Address (CLINT_State.Base_Address);
      Raw_Off  : constant Integer_Address :=
         Base_Int
       + Integer_Address (Natural (CLINT_State.MSIP_Offset))
       + Integer_Address (Natural (Hart_ID * 4));
      Addr     : constant Address := To_Address (Raw_Off);
      MSIP_Reg : Reg_Ptr := Reg (Addr);
   begin
      if not CLINT_State.Enabled then
         return False;
      end if;
      return MSIP_Reg.all /= 0;
   end Read_Software_Interrupt;

   --------------------------------------------------------------------------
   --  Timer (mtime/mtimecmp) Management
   --------------------------------------------------------------------------
   function Get_MTime return Unsigned_64 is
      Base_Int : constant Integer_Address :=
        Integer_Address (CLINT_State.Base_Address);
      Raw_Off  : constant Integer_Address :=
         Base_Int + Integer_Address (Natural (CLINT_State.MTime_Offset));
      Addr     : constant Address := To_Address (Raw_Off);
      Time_Reg : Reg_Ptr := Reg (Addr);
   begin
      if not CLINT_State.Enabled then
         return 0;
      end if;
      return Time_Reg.all;
   end Get_MTime;

   procedure Set_Timer_Compare (
     Hart_ID : Unsigned_64;
     Time    : Unsigned_64
   ) is
      Base_Int : constant Integer_Address :=
        Integer_Address (CLINT_State.Base_Address);
      -- Compute offset = mtimecmp_offset + hart_id * 8
      Off_U64  : constant Unsigned_64 :=
        CLINT_State.MTimeCmp_Offset + Hart_ID * 8;
      Raw_Off  : constant Integer_Address :=
        Base_Int + Integer_Address (Natural (Off_U64));
      Addr     : constant Address := To_Address (Raw_Off);
      Cmp_Reg  : Reg_Ptr := Reg (Addr);
   begin
      if not CLINT_State.Enabled then
         Arch.Debug.Print ("clint.Set_Timer_Compare: disabled");
         return;
      end if;
      Cmp_Reg.all := Time;
      Memory_Barrier;
      Arch.Debug.Print (
         "clint.Set_Timer_Compare: hart=" &
         Unsigned_64'Image (Hart_ID) &
         " time=" & Unsigned_64'Image (Time)
      );
   end Set_Timer_Compare;

   function Get_Timer_Compare (
     Hart_ID : Unsigned_64
   ) return Unsigned_64 is
      Base_Int : constant Integer_Address :=
        Integer_Address (CLINT_State.Base_Address);
      Off_U64  : constant Unsigned_64 :=
        CLINT_State.MTimeCmp_Offset + Hart_ID * 8;
      Raw_Off  : constant Integer_Address :=
        Base_Int + Integer_Address (Natural (Off_U64));
      Addr     : constant Address := To_Address (Raw_Off);
      Cmp_Reg  : Reg_Ptr := Reg (Addr);
   begin
      if not CLINT_State.Enabled then
         return 0;
      end if;
      return Cmp_Reg.all;
   end Get_Timer_Compare;

end Arch.CLINT;
