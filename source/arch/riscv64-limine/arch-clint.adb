--  arch-clint.adb: Implementation of Core Local Interruptor (CLINT) utilities.
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

with System.Machine_Code; use System.Machine_Code;
with Arch.Debug; -- For debug printing
with Ada.Unchecked_Conversion; -- For Address and Unsigned_64 conversions

package body Arch.CLINT is

   --  Address conversions.
   function Address_To_U64 is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Unsigned_64);

   function U64_To_Address is new Ada.Unchecked_Conversion
     (Source => Unsigned_64, Target => System.Address);

   --  Configuration API.
   procedure Set_CLINT_Configuration
     (Base_Address     : System.Address := System'To_Address(16#02000000#);
      MSIP_Offset      : Unsigned_64    := 0;
      MTime_Offset     : Unsigned_64    := 16#BFF8#;
      MTimecmp_Offset  : Unsigned_64    := 16#4000#;
      Enabled          : Boolean        := True) is
   begin
      CLINT_State.Base_Address    := Base_Address;
      CLINT_State.MSIP_Offset     := MSIP_Offset;
      CLINT_State.MTime_Offset    := MTime_Offset;
      CLINT_State.MTimecmp_Offset := MTimecmp_Offset;
      CLINT_State.Enabled         := Enabled;

      Arch.Debug.Print("CLINT: Configuration updated");
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

   --  Memory Barrier.
   procedure Memory_Barrier is
   begin
      Asm ("fence", Volatile => True, Clobber => "memory");
      Arch.Debug.Print("CLINT: Memory barrier executed");
   end Memory_Barrier;

   --  MMIO Access.
   function To_Reg_Ptr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Reg_Ptr);

   function Reg (Addr : System.Address) return Reg_Ptr is
   begin
      return To_Reg_Ptr(Addr);
   end Reg;

   --  Software Interrupts.
   procedure Set_Software_Interrupt (Hart_ID : Unsigned_64; Value : Boolean) is
      Addr : constant System.Address := U64_To_Address(
        Address_To_U64(Get_CLINT_Base) + Get_MSIP_Offset + Hart_ID * 4);
      R : Reg_Ptr := Reg(Addr);
   begin
      if Value then
         R.all := 1;
      else
         R.all := 0;
      end if;

      Arch.Debug.Print("CLINT: Software interrupt set for Hart_ID = " &
        Unsigned_64'Image(Hart_ID));
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Set_Software_Interrupt(Hart_ID, False);
   end Clear_Software_Interrupt;

   function Read_Software_Interrupt (Hart_ID : Unsigned_64) return Boolean is
      Addr : constant System.Address := U64_To_Address(
        Address_To_U64(Get_CLINT_Base) + Get_MSIP_Offset + Hart_ID * 4);
      R : Reg_Ptr := Reg(Addr);
   begin
      return R.all /= 0;
   end Read_Software_Interrupt;

   --  Timer.
   function Get_MTime return Unsigned_64 is
      Addr : constant System.Address := U64_To_Address(
      Address_To_U64(Get_CLINT_Base) + Get_MTime_Offset);
      R : Reg_Ptr := Reg(Addr);
   begin
      return Unsigned_64(R.all); -- Explicit conversion
   end Get_MTime;

   procedure Set_Timer_Compare (Hart_ID : Unsigned_64; Time : Unsigned_64) is
      Addr : constant System.Address := U64_To_Address(
        Address_To_U64(Get_CLINT_Base) + Get_MTimecmp_Offset + Hart_ID * 8);
      R : Reg_Ptr := Reg(Addr);
   begin
      R.all := Reg_Type(Time); -- Explicit conversion to match the expected type
      Arch.Debug.Print("CLINT: Timer compare set for Hart_ID = " &
        Unsigned_64'Image(Hart_ID));
   end Set_Timer_Compare;

   function Get_Timer_Compare (Hart_ID : Unsigned_64) return Unsigned_64 is
      Addr : constant System.Address := U64_To_Address(
        Address_To_U64(Get_CLINT_Base) + Get_MTimecmp_Offset + Hart_ID * 8);
      R : Reg_Ptr := Reg(Addr);
   begin
      return Unsigned_64 (R.all); -- Explicit conversion
   end Get_Timer_Compare;

end Arch.CLINT;