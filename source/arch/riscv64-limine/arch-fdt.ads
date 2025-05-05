--  arch-fdt.ads: Flattened Device Tree parser for riscv64-limine
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

package Arch.FDT is
   pragma Style_Checks (Off);

   --  Max number of Reg entries in a 'reg' property
   Max_Reg_Entries : constant Positive := 16;

   --  A single (address, size) tuple from a 'reg' property
   type Reg_Entry is record
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64;
   end record
     with Convention => C;

   --  Fixed-capacity vector of Reg_Entry
   type Reg_Vector is record
      Entries : array (Positive range 1 .. Max_Reg_Entries) of Reg_Entry;
      Length  : Natural := 0;
   end record;

   --  Append a Reg_Entry to Reg_Vector (no-op on overflow)
   procedure Append_Reg
     (Vec : in out Reg_Vector;
      E   : in     Reg_Entry);

   --  Interrupt specifier cells (e.g. <irq#, flags>)
   Max_Int_Cells : constant Positive := 8;
   type Int_Cells is array (Positive range 1 .. Max_Int_Cells) of Interfaces.Unsigned_32;
   type Int_Vector is record
      Cells  : Int_Cells;
      Length : Natural := 0;
   end record;

   --  Append an interrupt cell to Int_Vector (no-op on overflow)
   procedure Append_Int_Cell
     (Vec : in out Int_Vector;
      C   : in     Interfaces.Unsigned_32);

   --  Opaque handle to an open DTB
   type Handle is limited private;

   --  Map the DTB blob at Base_Address.  Returns a handle if magic valid
   function Open
     (Base_Address : System.Address)
      return Handle;

   --  Release resources (if any)
   procedure Close
     (H : in out Handle);

   --  Find the first node whose 'compatible' property includes the given string.
   --  Returns System.Null_Address if not found.
   function Find_Node
     (H           : Handle;
      Compatible  : String)
      return System.Address;

   --  Read a 32-bit integer property; return Default if missing
   function Get_Property_U32
     (H        : Handle;
      Node     : System.Address;
      Name     : String;
      Default  : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32;

   --  Read the 'reg' property into a Reg_Vector
   function Get_Reg
     (H    : Handle;
      Node : System.Address)
      return Reg_Vector;

   --  Read an interrupt-specifier array from a property
   function Get_Interrupts
     (H    : Handle;
      Node : System.Address;
      Name : String)
      return Int_Vector;

private
   --  Internal handle stores DTB base and block offsets
   type Handle is record
      Base_Address : System.Address;
      Off_Struct   : Interfaces.Unsigned_32;
      Off_Strings  : Interfaces.Unsigned_32;
   end record;

end Arch.FDT;