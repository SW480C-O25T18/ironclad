--  arch-exceptions.ads: Specification of Core Local Interruptor (CLINT) utilities.
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
--  along with this program.  If not, see <http://www.gnu.

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;
with Arch.Debug;

package body Arch.CLINT with SPARK_Mode => Off is

   --  Simple record to hold CLINT settings (no protected types)
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

   --  Convert System.Address to Unsigned_64 for printing
   function Address_To_U64 is new Ada.Unchecked_Conversion (
      Source => System.Address,
      Target => Unsigned_64
   );

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

   --  Volatile register access
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr is access all Reg_Type;
   function To_Reg_Ptr is new Ada.Unchecked_Conversion(
      Source => System.Address, 
      Target => Reg_Ptr
      );

   function Reg (Abs_Addr : System.Address) return Reg_Ptr is
   begin
      Arch.Debug.Print (
         "Reg: Addr = "
         & Unsigned_64'Image (Address_To_U64 (Abs_Addr))
      );
      return To_Reg_Ptr(Abs_Addr);
   end Reg;

   procedure Memory_Barrier is
   begin
      Arch.Debug.Print ("Memory barrier Start");
      Machine_Code.FENCE;
      Arch.Debug.Print ("Memory barrier End");
   end Memory_Barrier;

   procedure Set_Software_Interrupt (
      Hart_ID : Unsigned_64;
      Value   : Boolean
   ) is
      type MSIP_Type is new Unsigned_32;
      pragma Volatile (MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base
         + To_Address (
            Storage_Elements.Integer_Address(
               Get_MSIP_Offset + Hart_ID * 4));
      MSIP_Reg : MSIP_Ptr := MSIP_Ptr'Unchecked_Conversion(Addr);
   begin
      if not CLINT_Enabled then
         return;
      end if;
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
      Addr : constant System.Address :=
         Get_CLINT_Base
         + To_Address (
            Storage_Elements.Integer_Address(
               Get_MSIP_Offset + Hart_ID * 4));
      MSIP_Reg : MSIP_Ptr := MSIP_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         return False;
      end if;
      return MSIP_Reg.all /= 0;
   end Read_Software_Interrupt;

   function Get_MTime return Unsigned_64 is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;
      Addr : constant Address :=
         Get_CLINT_Base
            + To_Address(
               Storage_Elements.Integer_Address (
                  Get_MSIP_Offset + Hart_ID * 4)
               );
      Time_Reg : Time_Ptr := Time_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         return 0;
      end if;
      return Time_Reg.all;
   end Get_MTime;

   procedure Set_Timer_Compare (
      Hart_ID : Unsigned_64;
      Time    : Unsigned_64
   ) is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base
         + To_Address (Get_MTimecmp_Offset + Hart_ID * 8);
      Time_Reg : Time_Ptr := Time_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         return;
      end if;
      Time_Reg.all := Time;
      Memory_Barrier;
   end Set_Timer_Compare;

   function Get_Timer_Compare (
      Hart_ID : Unsigned_64
   ) return Unsigned_64 is
      type Time_Type is new Unsigned_64;
      pragma Volatile (Time_Type);
      type Time_Ptr is access all Time_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base
         + To_Address (Get_MTimecmp_Offset + Hart_ID * 8);
      Time_Reg : Time_Ptr := Time_Ptr (Addr);
   begin
      if not CLINT_Enabled then
         return 0;
      end if;
      return Time_Reg.all;
   end Get_Timer_Compare;

end Arch.CLINT;
