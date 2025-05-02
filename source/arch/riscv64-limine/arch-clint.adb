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

with System;
with System.Storage_Elements;
with Interfaces;
with Ada.Unchecked_Conversion;
with System.Machine_Code;
with Arch.Debug;

package body Arch.CLINT with SPARK_Mode => Off is

   subtype Address                is System.Address;
   subtype Integer_Address       is System.Storage_Elements.Integer_Address;
   subtype Unsigned_64           is Interfaces.Unsigned_64;
   subtype Unsigned_32           is Interfaces.Unsigned_32;

   --  CLINT configuration record
   type CLINT_Rec is record
      Base_Address    : Address;
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

   --  Helpers to convert between Address and Unsigned_64
   function Addr_To_U64 is new Ada.Unchecked_Conversion (Source => Address,
                                                          Target => Unsigned_64);
   function U64_To_Addr is new Ada.Unchecked_Conversion (Source => Unsigned_64,
                                                        Target => Address);

   --  Volatile register type
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr  is access all Reg_Type;

   --  Convert an absolute address to a volatile pointer
   function To_Reg_Ptr is new Ada.Unchecked_Conversion (Source => Address,
                                                       Target => Reg_Ptr);

   function Reg (Abs : Address) return Reg_Ptr is
   begin
      Arch.Debug.Print ("CLINT.Reg: 0x" &
         Interfaces.Unsigned_64'Image (Addr_To_U64 (Abs)));
      return To_Reg_Ptr (Abs);
   end Reg;

   --  Memory barrier (RISC-V FENCE)
   procedure Memory_Barrier is
   begin
      System.Machine_Code.Asm ("fence", Volatile => True, Clobber => "memory");
      Arch.Debug.Print ("CLINT.Memory_Barrier");
   end Memory_Barrier;

   -----------------------------------------------------------------------------
   --  Software interrupt (MSIP)
   -----------------------------------------------------------------------------

   procedure Set_Software_Interrupt (Hart_ID : Unsigned_64;
                                     Value   : Boolean) is
      --  MSIP is a 32-bit register at Base + MSIP_Offset + 4*Hart_ID
      type MSIP_Type is new Interfaces.Unsigned_32;
      pragma Volatile (MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;
      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (Source => Address,
                                                           Target => MSIP_Ptr);

      Base_Int  : constant Integer_Address :=
                    Integer_Address (CLINT_State.Base_Address);
      Raw_Off   : constant Integer_Address :=
                    Base_Int
                  + Integer_Address (CLINT_State.MSIP_Offset)
                  + Integer_Address (Hart_ID * 4);
      MSIP_Addr : constant Address := System.Storage_Elements.To_Address (Raw_Off);
      Reg_P     : MSIP_Ptr  := To_MSIP_Ptr (MSIP_Addr);
   begin
      if not CLINT_State.Enabled then
         Arch.Debug.Print ("CLINT.Set_Software_Interrupt: disabled");
         return;
      end if;

      if Value then
         Reg_P.all := 1;
      else
         Reg_P.all := 0;
      end if;

      Arch.Debug.Print ("CLINT.Set_Software_Interrupt: Hart=" &
         Unsigned_64'Image (Hart_ID) & " => " & Boolean'Image (Value));

      Memory_Barrier;
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Set_Software_Interrupt (Hart_ID, False);
   end Clear_Software_Interrupt;

   function Read_Software_Interrupt (Hart_ID : Unsigned_64) return Boolean is
      --  same address calc as Set
      type MSIP_Type is new Interfaces.Unsigned_32;
      pragma Volatile (MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;
      function To_MSIP_Ptr is new Ada.Unchecked_Conversion (Source => Address,
                                                           Target => MSIP_Ptr);

      Base_Int  : constant Integer_Address :=
                    Integer_Address (CLINT_State.Base_Address);
      Raw_Off   : constant Integer_Address :=
                    Base_Int
                  + Integer_Address (CLINT_State.MSIP_Offset)
                  + Integer_Address (Hart_ID * 4);
      MSIP_Addr : constant Address := System.Storage_Elements.To_Address (Raw_Off);
      Reg_P     : MSIP_Ptr  := To_MSIP_Ptr (MSIP_Addr);
   begin
      if not CLINT_State.Enabled then
         return False;
      end if;
      return Reg_P.all /= 0;
   end Read_Software_Interrupt;

   -----------------------------------------------------------------------------
   --  Timer (MTIME & MTIMECMP)
   -----------------------------------------------------------------------------

   function Get_MTime return Unsigned_64 is
      type T is new Unsigned_64; pragma Volatile (T);
      type TP is access all T;
      function To_TP is new Ada.Unchecked_Conversion (Source => Address,
                                                     Target => TP);

      Base_Int  : constant Integer_Address :=
                    Integer_Address (CLINT_State.Base_Address);
      Raw_Off   : constant Integer_Address :=
                    Base_Int + Integer_Address (CLINT_State.MTime_Offset);
      Addr      : constant Address := System.Storage_Elements.To_Address (Raw_Off);
      R         : TP      := To_TP (Addr);
   begin
      if not CLINT_State.Enabled then
         return 0;
      end if;
      return R.all;
   end Get_MTime;

   procedure Set_Timer_Compare (Hart_ID : Unsigned_64;
                                Time    : Unsigned_64) is
      type T is new Unsigned_64; pragma Volatile (T);
      type TP is access all T;
      function To_TP is new Ada.Unchecked_Conversion (Source => Address,
                                                     Target => TP);

      Base_Int  : constant Integer_Address :=
                    Integer_Address (CLINT_State.Base_Address);
      Raw_Off   : constant Integer_Address :=
                    Base_Int
                  + Integer_Address (CLINT_State.MTimecmp_Offset)
                  + Integer_Address (Hart_ID * 8);
      Addr      : constant Address := System.Storage_Elements.To_Address (Raw_Off);
      R         : TP      := To_TP (Addr);
   begin
      R.all := Time;
      Arch.Debug.Print ("CLINT.Set_Timer_Compare: Hart=" &
         Unsigned_64'Image (Hart_ID) & " => " & Unsigned_64'Image (Time));
      Memory_Barrier;
   end Set_Timer_Compare;

   function Get_Timer_Compare (Hart_ID : Unsigned_64) return Unsigned_64 is
      type T is new Unsigned_64; pragma Volatile (T);
      type TP is access all T;
      function To_TP is new Ada.Unchecked_Conversion (Source => Address,
                                                     Target => TP);

      Base_Int  : constant Integer_Address :=
                    Integer_Address (CLINT_State.Base_Address);
      Raw_Off   : constant Integer_Address :=
                    Base_Int
                  + Integer_Address (CLINT_State.MTimecmp_Offset)
                  + Integer_Address (Hart_ID * 8);
      Addr      : constant Address := System.Storage_Elements.To_Address (Raw_Off);
      R         : TP      := To_TP (Addr);
   begin
      if not CLINT_State.Enabled then
         return 0;
      end if;
      return R.all;
   end Get_Timer_Compare;

end Arch.CLINT;
