--  arch-fdt.ads: Flattened Device Tree parser for riscv64-limine
--  Provides Device Tree Blob (DTB) parsing,
--  node lookup, property and reg/interrupt readers.
--  Fully compliant with the official FDT v0.4/v1.4 spec.
--
--  Copyright (C) 2025 Sean C. Weeks
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
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

with System;
with Interfaces;
with Arch.Debug;
with Lib.Panic;

package Arch.FDT is
   pragma Style_Checks (Off);
   pragma Preelaborate;

   --  Basic subtypes
   subtype U32   is Interfaces.Unsigned_32;
   subtype U64   is Interfaces.Unsigned_64;
   subtype Addr  is System.Address;

   --  FDT token constants (§5.4.2)
   FDT_BEGIN_NODE : constant U32 := 1;
   FDT_END_NODE   : constant U32 := 2;
   FDT_PROP       : constant U32 := 3;
   FDT_NOP        : constant U32 := 4;
   FDT_END        : constant U32 := 9;

   --  Header offsets (§5.2)
   Header_Magic_Off       : constant := 0;
   Header_TotalSize_Off   : constant := 4;
   Header_Off_Struct      : constant := 8;
   Header_Off_Strings     : constant := 12;
   Header_Off_Reserve     : constant := 16;
   Header_Version_Off     : constant := 20;
   Header_LastComp_Off    : constant := 24;
   Header_BootHart_Off    : constant := 28;
   Header_SizeStrings_Off : constant := 32;
   Header_SizeStruct_Off  : constant := 36;

   --  Magic value (§5.2)
   DTB_Magic : constant U32 := 16#D00DFEED#;

   --  Opaque DTB handle storing header and block info
   type Handle is record
      Base_Address       : Addr;
      Total_Size         : U32;
      Off_Struct         : U32;
      Off_Strings        : U32;
      Off_Reserve        : U32;
      Version            : U32;
      Last_Comp_Version  : U32;
      Boot_CPUID         : U32;
      Size_Struct        : U32;
      Size_Strings       : U32;
      Struct_End         : Addr;
   end record;

   --  Initialize parser with DTB at Base; returns False on invalid blob
   function Initialize (Base_Address : Addr) return Boolean;

   --  Release resources (no-op)
   procedure Close (H : in out Handle);

   --  Boot hart identifier from header
   function Boot_CPUID (H : Handle) return U32;

   --  Spec version and compatibility
   function Version (H : Handle) return U32;
   function Last_Compatible (H : Handle) return U32;

   --  Size of structure and strings blocks
   function Structure_Size (H : Handle) return U32;
   function Strings_Size   (H : Handle) return U32;

   --  Node lookup by 'compatible' (binary-search on cache)
   function Find_Node (H : Handle; Compatible : String) return Addr;
   function Find_Node (Compatible : String) return Addr;

   --  Property readers
   function Get_Property_U32
     (H       : Handle;
      Node    : Addr;
      Name    : String;
      Default : U32)
      return U32;

   function Get_Property_U64
     (H       : Handle;
      Node    : Addr;
      Name    : String;
      Default : U64)
      return U64;

   function Get_Property_Array_U32
     (H    : Handle;
      Node : Addr;
      Name : String)
      return Int_Vector;

   --  'reg' and 'interrupts' readers
   function Get_Reg (H : Handle; Node : Addr) return Reg_Vector;
   function Get_Interrupts (H : Handle; Node : Addr; Name : String) return Int_Vector;

private
   --  Fixed-capacity structures (no unconstrained arrays)
   Max_Compat   : constant := 64;
   Max_Compat_L : constant := 64;
   Max_Reg      : constant := 32;
   Max_Int_Cells: constant := 8;

   type Compat_String is record
      Length : Natural;
      Data   : String (1 .. Max_Compat_L);
   end record;

   type Compat_Entry is record
      Compatible : Compat_String;
      Offset     : Addr;
   end record;

   type Compat_Array is array (Positive range 1 .. Max_Compat) of Compat_Entry;
   Compat_Cache : Compat_Array;
   Compat_Count : Natural := 0;

   --  Reg and interrupt vectors
   type Reg_Entry is record
      Address : U64;
      Size    : U64;
   end record;

   type Reg_Vector is record
      Entries : array (Positive range 1 .. Max_Reg) of Reg_Entry;
      Length  : Natural := 0;
   end record;

   type Int_Vector is record
      Cells  : array (Positive range 1 .. Max_Int_Cells) of U32;
      Length : Natural := 0;
   end record;

   --  Low-level helper signatures
   function Read8      (A : Addr)               return Interfaces.Unsigned_8;
   function Read_BE32  (A : Addr)               return U32;
   function Read_BE64  (A : Addr)               return U64;
   function Align4     (A : Addr)               return Addr;

   --  Append helpers
   procedure Append_Reg      (Vec : in out Reg_Vector; E : in Reg_Entry);
   procedure Append_Int_Cell (Vec : in out Int_Vector; C : in U32);

   --  Persistent handle
   DTB_Handle : Handle;
end Arch.FDT;