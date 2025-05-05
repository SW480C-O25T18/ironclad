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
--  Ironclad is distributed in the hope that it will be useful,
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
with Arch.CPU;       -- for reading hart ID via CSR
with Arch.SBI;       -- for SBI timer and IPI calls

package Arch.Clint is
   pragma Pure;

   --  Stride constants per CLINT spec
   MSIP_Stride     : constant Interfaces.Unsigned_32 := 4;
   MTIMECMP_Stride : constant Interfaces.Unsigned_32 := 8;

   --  Initialize the CLINT: locate via FDT or prepare SBI fallback
   procedure Initialize;

   --  Software IPI (MSIP) API
   procedure Set_Software_Interrupt (Target : Interfaces.Unsigned_32);
   procedure Clear_Software_Interrupt (Target : Interfaces.Unsigned_32);
   function  Get_Software_Interrupt (Target : Interfaces.Unsigned_32) return Boolean;

   --  Timer API (MTIME/MTIMECMP)
   procedure Set_Timer_Interrupt (Value : Interfaces.Unsigned_64);
   procedure Clear_Timer_Interrupt;
   function  Read_Timer return Interfaces.Unsigned_64;

   --  Optional CLINT resynchronization for power gate recovery
   procedure Resync (Reference  : System.Address);

private
   MSIP_Base     : System.Address := System.Null_Address;
   MTIME_Base    : System.Address := System.Null_Address;
   MTIMECMP_Base : System.Address := System.Null_Address;
end Arch.Clint;
