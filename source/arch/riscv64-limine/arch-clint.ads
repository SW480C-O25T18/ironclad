--  arch-clint.ads: Specification of Core Local Interruptor (CLINT) utilities.
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
with Interfaces; use Interfaces;

package Arch.CLINT is

   type CLINT_State_Type is record
      Base_Address    : System.Address;
      MSIP_Offset     : Unsigned_64;
      MTime_Offset    : Unsigned_64;
      MTimecmp_Offset : Unsigned_64;
      Enabled         : Boolean;
   end record;

   CLINT_State : CLINT_State_Type;

   --  Set CLINT configuration dynamically.
   procedure Set_CLINT_Configuration
     (Base_Address     : System.Address := System'To_Address(16#02000000#);
      MSIP_Offset      : Unsigned_64    := 0;
      MTime_Offset     : Unsigned_64    := 16#BFF8#;
      MTimecmp_Offset  : Unsigned_64    := 16#4000#;
      Enabled          : Boolean        := True);

   --  Getters for CLINT configuration.
   function Get_CLINT_Base return System.Address;
   function Get_MSIP_Offset return Unsigned_64;
   function Get_MTime_Offset return Unsigned_64;
   function Get_MTimecmp_Offset return Unsigned_64;
   function CLINT_Enabled return Boolean;

   --  Software interrupt management.
   procedure Set_Software_Interrupt (Hart_ID : Unsigned_64; Value : Boolean);
   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64);
   function Read_Software_Interrupt (Hart_ID : Unsigned_64) return Boolean;

   --  Timer management.
   function Get_MTime return Unsigned_64;
   procedure Set_Timer_Compare (Hart_ID : Unsigned_64; Time : Unsigned_64);
   function Get_Timer_Compare (Hart_ID : Unsigned_64) return Unsigned_64;

   --  Memory barrier.
   procedure Memory_Barrier;

   --  Read and write operations.
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr is access all Reg_Type;

   function Reg (Addr : System.Address) return Reg_Ptr;

end Arch.CLINT;