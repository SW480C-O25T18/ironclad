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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;
with Arch.Debug;

package body Arch.CLINT with SPARK_Mode => Off is

   -----------------------------------------------------------------------------
   --  CLINT Configuration Record
   -----------------------------------------------------------------------------
   type CLINT_Rec is record
      Base_Address    : System.Address;
      MSIP_Offset     : Unsigned_64;
      MTime_Offset    : Unsigned_64;
      MTimecmp_Offset : Unsigned_64;
      Enabled         : Boolean;
   end record;

   CLINT_State : CLINT_Rec := (
      Base_Address    => System'To_Address (16#02000000#),
      MSIP_Offset     => 0,
      MTime_Offset    => 16#BFF8#,
      MTimecmp_Offset => 16#4000#,
      Enabled         => True
   );

   -----------------------------------------------------------------------------
   --  Helper Functions for Address Conversion
   -----------------------------------------------------------------------------
   function Address_To_U64 is new Ada.Unchecked_Conversion (
      Source => System.Address,
      Target => Unsigned_64
   );

   function U64_To_Address is new Ada.Unchecked_Conversion (
      Source => Unsigned_64,
      Target => System.Address
   );

   -----------------------------------------------------------------------------
   --  CLINT Configuration Procedures
   -----------------------------------------------------------------------------
   procedure Set_CLINT_Configuration (
      Base_Address     : System.Address;
      MSIP_Offset      : Unsigned_64;
      MTime_Offset     : Unsigned_64;
      MTimecmp_Offset  : Unsigned_64;
      Enabled          : Boolean
   ) is
   begin
      CLINT_State.Base_Address    := Base_Address;
      CLINT_State.MSIP_Offset     := MSIP_Offset;
      CLINT_State.MTime_Offset    := MTime_Offset;
      CLINT_State.MTimecmp_Offset := MTimecmp_Offset;
      CLINT_State.Enabled         := Enabled;
      Arch.Debug.Print ("Set_CLINT_Configuration: Configuration updated");
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
   --  Volatile Register Access
   -----------------------------------------------------------------------------
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr is access all Reg_Type;

   function To_Reg_Ptr is new Ada.Unchecked_Conversion (
      Source => System.Address,
      Target => Reg_Ptr
   );

   function Reg (Abs_Addr : System.Address) return Reg_Ptr is
   begin
      Arch.Debug.Print (
         "Reg: Addr = " & Unsigned_64'Image (Address_To_U64 (Abs_Addr))
      );
      return To_Reg_Ptr (Abs_Addr);
   end Reg;

   -----------------------------------------------------------------------------
   --  Memory Barrier
   -----------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      Arch.Debug.Print ("Memory_Barrier: Executing memory barrier");
      Asm ("fence", Volatile => True, Clobber => "memory");
      Arch.Debug.Print ("Memory_Barrier: Memory barrier executed");
   end Memory_Barrier;

   -----------------------------------------------------------------------------
   --  Software Interrupt Management
   -----------------------------------------------------------------------------
   procedure Set_Software_Interrupt (
      Hart_ID : Unsigned_64;
      Value   : Boolean
   ) is
      type MSIP_Type is new Unsigned_32;
      pragma Volatile (MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;

      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => MSIP_Ptr
      );

      Base_Int : constant Unsigned_64 := Address_To_U64 (Get_CLINT_Base);
      Offset   : constant Unsigned_64 :=
        Base_Int + Address_To_U64 (Get_MSIP_Offset) + Hart_ID * 4;
      Addr     : constant System.Address := U64_To_Address (Offset);
      MSIP_Reg : MSIP_Ptr := To_MSIP_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         Arch.Debug.Print ("Set_Software_Interrupt: CLINT is disabled");
         return;
      end if;

      Arch.Debug.Print ("Set_Software_Interrupt: Setting MSIP for Hart_ID = "
         & Unsigned_64'Image (Hart_ID) & " to " & Boolean'Image (Value));

      if Value then
         MSIP_Reg.all := 1;
      else
         MSIP_Reg.all := 0;
      end if;

      Memory_Barrier;
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Set_Software_Interrupt (Hart_ID, False);
   end Clear_Software_Interrupt;

   function Read_Software_Interrupt (
      Hart_ID : Unsigned_64
   ) return Boolean is
      type MSIP_Type is new Unsigned_32;
      pragma Volatile (MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;

      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => MSIP_Ptr
      );

      Base_Int : constant Unsigned_64 := Address_To_U64 (Get_CLINT_Base);
      Offset   : constant Unsigned_64 :=
        Base_Int + Address_To_U64 (Get_MSIP_Offset) + Hart_ID * 4;
      Addr     : constant System.Address := U64_To_Address (Offset);
      MSIP_Reg : MSIP_Ptr := To_MSIP_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         Arch.Debug.Print ("Read_Software_Interrupt: CLINT is disabled");
         return False;
      end if;

      return MSIP_Reg.all /= 0;
   end Read_Software_Interrupt;

   -----------------------------------------------------------------------------
   --  Timer Management
   -----------------------------------------------------------------------------
   function Get_MTime return Unsigned_64 is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;

      function To_Time_Ptr is new Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => Time_Ptr
      );

      Addr     : constant System.Address :=
        U64_To_Address (Address_To_U64 (Get_CLINT_Base) + Address_To_U64 (Get_MTime_Offset));
      Time_Reg : Time_Ptr := To_Time_Ptr (Addr);
   begin
      return Time_Reg.all;
   end Get_MTime;

   procedure Set_Timer_Compare (
      Hart_ID : Unsigned_64;
      Time    : Unsigned_64
   ) is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;

      function To_Time_Ptr is new Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => Time_Ptr
      );

      Addr     : constant System.Address :=
        U64_To_Address (Address_To_U64 (Get_CLINT_Base) + Address_To_U64 (Get_MTimecmp_Offset) + Hart_ID * 8);
      Time_Reg : Time_Ptr := To_Time_Ptr (Addr);
   begin
      Arch.Debug.Print ("Set_Timer_Compare: Setting timer compare for Hart_ID = "
         & Unsigned_64'Image (Hart_ID) & " to " & Unsigned_64'Image (Time));

      Time_Reg.all := Time;
      Memory_Barrier;
   end Set_Timer_Compare;

   function Get_Timer_Compare (
      Hart_ID : Unsigned_64
   ) return Unsigned_64 is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;

      function To_Time_Ptr is new Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => Time_Ptr
      );

      Addr     : constant System.Address :=
        U64_To_Address (Address_To_U64 (Get_CLINT_Base) + Address_To_U64 (Get_MTimecmp_Offset) + Hart_ID * 8);
      Time_Reg : Time_Ptr := To_Time_Ptr (Addr);
   begin
      return Time_Reg.all;
   end Get_Timer_Compare;

end Arch.CLINT;