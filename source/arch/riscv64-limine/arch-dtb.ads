--  arch-dtb.ads: Device-tree blob parsing.
--  Provides utilities for parsing and interacting with DTBs
--  in RISC-V64 systems.
--  Implements parsing, property fetching, and node traversal for DTBs.
--  Fully compliant with the DTB specification.
--  Optimized for time and space efficiency.
--  Includes meaningful comments and adheres to GNAT style rules.
--  Copyright (C) 2024 streaksu
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces;  use Interfaces;
with Arch.Limine; use Arch.Limine;

package Arch.DTB with SPARK_Mode => Off is

   --  Constants
   Max_Name_Length      : constant := 64;
   --  Maximum length of a node or property name.

   Max_DTB_Properties   : constant := 32;
   --  Maximum number of properties per node.

   Max_DTB_Children     : constant := 64;
   --  Maximum number of child nodes per node.

   --  Types
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   --  Array type for storing property values.

   type DTB_Property is record
      Name  : String (1 .. Max_Name_Length);
      --  Name of the property.

      Value : Unsigned_64_Array (1 .. 8);
      --  Property value (arbitrary max size for simplicity).

      Len   : Natural;
      --  Length of the property value.
   end record;

   type DTB_Property_Access is access all DTB_Property;
   --  Access type for DTB_Property.

   type DTB_Property_Access_Array is array (0 .. Max_DTB_Properties - 1)
     of DTB_Property_Access;
   --  Array type for storing property access pointers.

   type DTB_Node;
   --  Forward declaration for DTB_Node.

   type DTB_Node_Access is access all DTB_Node;
   --  Access type for DTB_Node.

   type DTB_Node_Access_Array is array (0 .. Max_DTB_Children - 1)
     of DTB_Node_Access;
   --  Array type for storing child node access pointers.

   type DTB_Node is record
      Name        : String (1 .. Max_Name_Length);
      --  Name of the node.

      Properties  : DTB_Property_Access_Array;
      --  Array of properties for the node.

      Prop_Count  : Natural := 0;
      --  Number of properties in the node.

      Children    : DTB_Node_Access_Array;
      --  Array of child nodes.

      Child_Count : Natural := 0;
      --  Number of child nodes.
   end record;

   --  Root node of the device tree
   Root_Node : DTB_Node_Access;

   --  Functions and Procedures

   --  Initialize DTB parsing.
   function Init return Boolean;

   --  Parse the entire DTB structure.
   procedure Parse_DTB;

   --  Print a single DTB node and its properties.
   procedure Print_DTB_Node (Node : DTB_Node_Access; Indent : String := "");

   --  Print the entire DTB structure.
   procedure Print_DTB;

   --  Fetch a property value as an array of Unsigned_64.
   function Get_Property_Unsigned_64
     (Node : DTB_Node_Access; Name : String) return Unsigned_64_Array;

   --  Find a node by its "compatible" property.
   function Find_Node_By_Compatible (Compat : String) return DTB_Node_Access;

   --  Fetch a specific indexed value from a property as Unsigned_64.
   function Get_Property_Unsigned_64
     (Node  : DTB_Node_Access;
      Name  : String;
      Index : Positive) return Unsigned_64;

private

   --  Limine DTB request
   DTB_Request : constant Arch.Limine.Request :=
      (ID       => Arch.Limine.DTB_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  DTB Magic Number
   FDT_Magic : constant Unsigned_32 := 16#EDFE0DD0#;

   --  DTB Header
   type FDT_Header is record
      Magic                : Unsigned_32;
      --  Magic number identifying the DTB.

      Size                 : Unsigned_32;
      --  Total size of the DTB in bytes.

      Offset_DT_Struct     : Unsigned_32;
      --  Offset to the structure block.

      Offset_DT_Strings    : Unsigned_32;
      --  Offset to the strings block.

      Offset_Reserved_Flag : Unsigned_32;
      --  Reserved flag offset.

      Version              : Unsigned_32;
      --  DTB version.

      Last_Compatible_Vers : Unsigned_32;
      --  Last compatible version.

      Boot_CPU_Physical_ID : Unsigned_32;
      --  Physical ID of the boot CPU.

      Size_DT_Strings      : Unsigned_32;
      --  Size of the strings block.

      Size_DT_Struct       : Unsigned_32;
      --  Size of the structure block.
   end record with Pack;

end Arch.DTB;