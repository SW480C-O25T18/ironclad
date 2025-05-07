--  arch-clint.ads: CLINT driver for riscv64-limine, SBI-friendly
--  Copyright (C) 2025 Sean Weeks
--  
--  This file is part of Ironclad.
--  
--  Ironclad is free software: you can redistribute it and/or modify
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
--  along with Ironclad.  If not, see <https://www.gnu.org/licenses/>.

with System;
with Interfaces;
with Arch.Debug;
with Lib.Panic;
with Arch.FDT;
with Arch.CPU;
with Arch.SBI;

package Arch.CLINT is
   pragma Pure;

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   subtype Address is System.Address;

   --  CLINT register layout constants
   MSIP_Stride     : constant U32 := 4;
   MTIME_Offset    : constant U64 := 16#4000#;
   MTIMECMP_Offset : constant U64 := 16#4008#;

   --  Legacy SBI extension IDs
   Legacy_EID_Time       : constant U64 := 16#00#;
   Legacy_EID_ClearIPI   : constant U64 := 16#03#;
   Legacy_EID_SendIPI    : constant U64 := 16#04#;

   --  Ratified SBI extension IDs
   Current_EID_TIME      : constant U64 := 16#54494D45#;  -- 'TIME'
   Current_EID_sPI       : constant U64 := 16#735049#;    -- 'sPI'

   --  Mode selection flags
   Use_SBI_Time  : Boolean := False;
   Use_MMIO_Time : Boolean := False;
   Use_SBI_IPI   : Boolean := False;
   Use_MMIO_IPI  : Boolean := False;

   --  CLINT overall enable
   function Is_Enabled return Boolean;

   --  Initialization
   procedure Initialize;

   --  Software IPI (MSIP)
   procedure Set_Software_Interrupt (Target : U32);
   procedure Clear_Software_Interrupt (Target : U32);
   function  Get_Software_Interrupt (Target : U32) return Boolean;

   --  Timer (MTIME/MTIMECMP)
   procedure Set_Timer_Interrupt (Value : U64);
   procedure Clear_Timer_Interrupt;
   function  Read_Timer return U64;

   --  Optional resync after power gating
   procedure Resync (Reference : Address);

private
   --  MMIO bases
   MSIP_Base     : Address := System.Null_Address;
   MTIME_Base    : Address := System.Null_Address;
   MTIMECMP_Base : Address := System.Null_Address;

   CLINT_Enabled   : Boolean := False;
   Timer_Interval  : U64 := 0;
end Arch.CLINT;