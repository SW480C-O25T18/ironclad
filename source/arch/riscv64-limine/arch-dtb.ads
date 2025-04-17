--  arch-dtb.ads: Device-tree blob parsing.
--  Copyright (C) 2024 streaksu
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
   
   Max_Name_Length      : constant := 64;
   Max_DTB_Properties   : constant := 32;
   Max_DTB_Children     : constant := 64;

   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;

   type DTB_Property_Access is access all Interfaces.Unsigned_32;
   type DTB_Property_Access_Array is array (0 .. Max_DTB_Properties - 1)
     of DTB_Property_Access;

   type DTB_Node;
   type DTB_Node_Access is access all DTB_Node;
   type DTB_Node_Access_Array is array (0 .. Max_DTB_Children - 1)
     of DTB_Node_Access;

   type Name_String is array (Positive range <>) of Character;
   pragma Convention (C, Name_String);

   type DTB_Node is record
      Name        : String (1 .. Max_Name_Length);
      Name_Len    : Natural := 0;
      Properties  : DTB_Property_Access_Array;
      Prop_Count  : Natural := 0;
      Children    : DTB_Node_Access_Array;
      Child_Count : Natural := 0;
   end record;

   function Init return Boolean;
   procedure Print_DTB_Node (Node   : DTB_Node_Access;
      Indent : String := ""); 

   function Get_Property_Unsigned_64
      (Node : DTB_Node_Access; Name : String)
      return Unsigned_64_Array;

   function Find_Node_By_Compatible
      (Compat : String) return DTB_Node_Access;

private

   DTB_Request : Arch.Limine.Request :=
      (ID       => Arch.Limine.DTB_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  It is DOODFEED, but that is big endian, this is little endian.
   FDT_Magic : constant Unsigned_32 := 16#EDFE0DD0#;

   type FDT_Header is record
      Magic                : Unsigned_32;
      Size                 : Unsigned_32;
      Offset_DT_Struct     : Unsigned_32;
      Offset_DT_Strings    : Unsigned_32;
      Offset_Reserved_Flag : Unsigned_32;
      Version              : Unsigned_32;
      Last_Compatible_Vers : Unsigned_32;
      Boot_CPU_Physical_ID : Unsigned_32;
      Size_DT_Strings       : Unsigned_32;
      Size_DT_Struct       : Unsigned_32;
   end record with Pack;
end Arch.DTB;
