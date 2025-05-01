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
--  Copyright (C) 2025 scweeks

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

package body Arch.CLINT with SPARK_Mode => Off is

   -----------------------------------------------------------------------------
   -- CLINT state record, matching the spec exactly
   -----------------------------------------------------------------------------
   type CLINT_Rec is record
      Base_Address    : System.Address;
      MSIP_Offset     : Unsigned_64;
      MTime_Offset    : Unsigned_64;
      MTimecmp_Offset : Unsigned_64;
      Enabled         : Boolean;
   end record;

   CLINT_State : CLINT_Rec := (
      Base_Address    => System'To_Address (16#0200_0000#),
      MSIP_Offset     => 0,
      MTime_Offset    => 16#BFF8#,
      MTimecmp_Offset => 16#4000#,
      Enabled         => True
   );

   -----------------------------------------------------------------------------
   -- Configuration routines
   -----------------------------------------------------------------------------
   procedure Set_CLINT_Configuration (
      Base_Address    : System.Address;
      MSIP_Offset     : Unsigned_64;
      MTime_Offset    : Unsigned_64;
      MTimecmp_Offset : Unsigned_64;
      Enabled         : Boolean
   ) is
   begin
      CLINT_State.Base_Address    := Base_Address;
      CLINT_State.MSIP_Offset     := MSIP_Offset;
      CLINT_State.MTime_Offset    := MTime_Offset;
      CLINT_State.MTimecmp_Offset := MTimecmp_Offset;
      CLINT_State.Enabled         := Enabled;
   end Set_CLINT_Configuration;

   function Get_CLINT_Base return System.Address is
   begin
      return CLINT_State.Base_Address;
   end Get_CLINT_Base;

   function Get_MSIP_Offset return Unsigned_64 is
   begin
      return CLINT_State.MSIP_Offset;
   end Get_MSIP_Offset;

   function Get_MTime_Offset return Unsigned_64 is
   begin
      return CLINT_State.MTime_Offset;
   end Get_MTime_Offset;

   function Get_MTimecmp_Offset return Unsigned_64 is
   begin
      return CLINT_State.MTimecmp_Offset;
   end Get_MTimecmp_Offset;

   function CLINT_Enabled return Boolean is
   begin
      return CLINT_State.Enabled;
   end CLINT_Enabled;

   -----------------------------------------------------------------------------
   -- Utility to print addresses as 64-bit values
   -----------------------------------------------------------------------------
   function Address_To_U64 is new Ada.Unchecked_Conversion (
      Source => System.Address,
      Target => Unsigned_64
   );

   -----------------------------------------------------------------------------
   -- A true RISC-V memory fence
   -----------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      Asm ("fence", Volatile => True, Clobber => "memory");
   end Memory_Barrier;

   -----------------------------------------------------------------------------
   -- Software-interrupt registers (MSIP)
   -----------------------------------------------------------------------------
   procedure Set_Software_Interrupt (
      Hart_ID : Unsigned_64;
      Value   : Boolean
   ) is
      type MSIP_Type is new Unsigned_32; pragma Volatile (MSIP_Type);
      type MSIP_Ptr  is access all MSIP_Type;
      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (
        Source => System.Address,
        Target => MSIP_Ptr
      );

      Base_Int : constant Integer_Address := To_Integer (Get_CLINT_Base);
      Off_Int  : constant Integer_Address :=
        Integer_Address (Get_MSIP_Offset) + Integer_Address (Hart_ID * 4);
      Addr     : constant System.Address := To_Address (Base_Int + Off_Int);
      MSIP_Reg : MSIP_Ptr := To_MSIP_Ptr (Addr);
   begin
      if not CLINT_State.Enabled then
         return;
      end if;
      MSIP_Reg.all := (if Value then 1 else 0);
      Memory_Barrier;
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Set_Software_Interrupt (Hart_ID, False);
   end Clear_Software_Interrupt;

   function Read_Software_Interrupt (
      Hart_ID : Unsigned_64
   ) return Boolean is
      type MSIP_Type is new Unsigned_32; pragma Volatile (MSIP_Type);
      type MSIP_Ptr  is access all MSIP_Type;
      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (
        Source => System.Address,
        Target => MSIP_Ptr
      );

      Base_Int : constant Integer_Address := To_Integer (Get_CLINT_Base);
      Off_Int  : constant Integer_Address :=
        Integer_Address (Get_MSIP_Offset) + Integer_Address (Hart_ID * 4);
      Addr     : constant System.Address := To_Address (Base_Int + Off_Int);
      MSIP_Reg : MSIP_Ptr := To_MSIP_Ptr (Addr);
   begin
      if not CLINT_State.Enabled then
         return False;
      end if;
      return MSIP_Reg.all /= 0;
   end Read_Software_Interrupt;

   -----------------------------------------------------------------------------
   -- Machine timer (MTIME) and compare (MTIMECMP)
   -----------------------------------------------------------------------------
   function Get_MTime return Unsigned_64 is
      type Time_Type is new Unsigned_64; pragma Volatile (Time_Type);
      type Time_Ptr  is access all Time_Type;
      function To_Time_Ptr is new Ada.Unchecked_Conversion (
        Source => System.Address,
        Target => Time_Ptr
      );

      Base_Int : constant Integer_Address := To_Integer (Get_CLINT_Base);
      Off_Int  : constant Integer_Address := Integer_Address (Get_MTime_Offset);
      Addr     : constant System.Address := To_Address (Base_Int + Off_Int);
      Time_Reg : Time_Ptr := To_Time_Ptr (Addr);
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
      type Cmp_Type is new Unsigned_64; pragma Volatile (Cmp_Type);
      type Cmp_Ptr  is access all Cmp_Type;
      function To_Cmp_Ptr is new Ada.Unchecked_Conversion (
        Source => System.Address,
        Target => Cmp_Ptr
      );

      Base_Int : constant Integer_Address := To_Integer (Get_CLINT_Base);
      Off_Int  : constant Integer_Address :=
        Integer_Address (Get_MTimecmp_Offset) + Integer_Address (Hart_ID * 8);
      Addr     : constant System.Address := To_Address (Base_Int + Off_Int);
      Cmp_Reg  : Cmp_Ptr := To_Cmp_Ptr (Addr);
   begin
      if not CLINT_State.Enabled then
         return;
      end if;
      Cmp_Reg.all := Time;
      Memory_Barrier;
   end Set_Timer_Compare;

   function Get_Timer_Compare (
      Hart_ID : Unsigned_64
   ) return Unsigned_64 is
      type Cmp_Type is new Unsigned_64; pragma Volatile (Cmp_Type);
      type Cmp_Ptr  is access all Cmp_Type;
      function To_Cmp_Ptr is new Ada.Unchecked_Conversion (
        Source => System.Address,
        Target => Cmp_Ptr
      );

      Base_Int : constant Integer_Address := To_Integer (Get_CLINT_Base);
      Off_Int  : constant Integer_Address :=
        Integer_Address (Get_MTimecmp_Offset) + Integer_Address (Hart_ID * 8);
      Addr     : constant System.Address := To_Address (Base_Int + Off_Int);
      Cmp_Reg  : Cmp_Ptr := To_Cmp_Ptr (Addr);
   begin
      if not CLINT_State.Enabled then
         return 0;
      end if;
      return Cmp_Reg.all;
   end Get_Timer_Compare;

end Arch.CLINT;
