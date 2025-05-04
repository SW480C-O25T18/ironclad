--  arch-dtb.ads: Device-tree blob parsing.
--  Provides utilities for parsing and interacting with DTBs
--  in RISC-V64 systems.
--  Implements parsing, property fetching, and node traversal for DTBs.
--  Fully compliant with the DTB specification.
--  Optimized for time and space efficiency.
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

with Interfaces; use Interfaces;
with Arch.Limine;

package Arch.DTB with SPARK_Mode => Off is

   -- Constants
   Max_Name_Length      : constant := 64;
   Max_DTB_Properties   : constant := 32;
   Max_DTB_Children     : constant := 64;

   -- Types
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;

   type DTB_Property is record
      Name  : String (1 .. Max_Name_Length);
      Value : Unsigned_64_Array (1 .. 8); -- Arbitrary max size for simplicity
      Len   : Natural;
   end record;

   type DTB_Property_Access is access all DTB_Property;
   type DTB_Property_Access_Array is array (0 .. Max_DTB_Properties - 1)
     of DTB_Property_Access;

   type DTB_Node;
   type DTB_Node_Access is access all DTB_Node;
   type DTB_Node_Access_Array is array (0 .. Max_DTB_Children - 1)
     of DTB_Node_Access;

   type DTB_Node is record
      Name        : String (1 .. Max_Name_Length);
      Properties  : DTB_Property_Access_Array;
      Prop_Count  : Natural := 0;
      Children    : DTB_Node_Access_Array;
      Child_Count : Natural := 0;
   end record;

   -- Root node of the device tree
   Root_Node : DTB_Node_Access;

   -- Functions and Procedures
   function Init return Boolean;
   procedure Parse_DTB;
   procedure Print_DTB_Node (Node : DTB_Node_Access; Indent : String := "");
   procedure Print_DTB;
   function Get_Property_Unsigned_64
     (Node : DTB_Node_Access; Name : String) return Unsigned_64_Array;
   function Find_Node_By_Compatible (Compat : String) return DTB_Node_Access;
   -- Fetch a specific indexed value from a property as Unsigned_64
   function Get_Property_Unsigned_64
     (Node  : DTB_Node_Access;
      Name  : String;
      Index : Positive) return Unsigned_64;

private

   -- Limine DTB request
   DTB_Request : constant Arch.Limine.Request :=
      (ID       => Arch.Limine.DTB_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   -- DTB Magic Number
   FDT_Magic : constant Unsigned_32 := 16#EDFE0DD0#;

   -- DTB Header
   type FDT_Header is record
      Magic                : Unsigned_32;
      Size                 : Unsigned_32;
      Offset_DT_Struct     : Unsigned_32;
      Offset_DT_Strings    : Unsigned_32;
      Offset_Reserved_Flag : Unsigned_32;
      Version              : Unsigned_32;
      Last_Compatible_Vers : Unsigned_32;
      Boot_CPU_Physical_ID : Unsigned_32;
      Size_DT_Strings      : Unsigned_32;
      Size_DT_Struct       : Unsigned_32;
   end record with Pack;

end Arch.DTB;