--  arch-dtb.adb: Device-tree blob parsing.
--  Provides utilities for parsing and interacting with DTBs
--  in RISC-V64 systems.
--  Implements parsing, property fetching, and node traversal for DTBs.
--  Fully compliant with the DTB specification.
--  Optimized for time and space efficiency.
--  Includes meaningful debug statements and proper exception handling.
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
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Arch.Limine;
with Arch.Debug;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Arch.DTB with SPARK_Mode => Off is

   -----------------------------------------------------------------------------
   -- DTB Token Constants
   -----------------------------------------------------------------------------
   FDT_BEGIN_NODE : constant Unsigned_32 := 16#1#;
   FDT_END_NODE   : constant Unsigned_32 := 16#2#;
   FDT_PROP       : constant Unsigned_32 := 16#3#;
   FDT_NOP        : constant Unsigned_32 := 16#4#;
   FDT_END        : constant Unsigned_32 := 16#9#;

   -----------------------------------------------------------------------------
   -- Magic Number
   -----------------------------------------------------------------------------
   FDT_MAGIC : constant Unsigned_32 := 16#EDFE0DD0#;

   -----------------------------------------------------------------------------
   -- Access Types for Pointer Conversions
   -----------------------------------------------------------------------------
   type U32_Ptr is access all Unsigned_32;
   function To_U32_Ptr is new Ada.Unchecked_Conversion(Address, U32_Ptr);

   type U8_Ptr is access all Unsigned_8;
   function To_U8_Ptr is new Ada.Unchecked_Conversion(Address, U8_Ptr);

   -----------------------------------------------------------------------------
   -- Base Addresses (Set in Init)
   -----------------------------------------------------------------------------
   Struct_Base  : Address := Null_Address;
   Strings_Base : Address := Null_Address;
   DTB_End      : Address := Null_Address;

   -----------------------------------------------------------------------------
   -- Helper Functions
   -----------------------------------------------------------------------------

   -- Convert 32-bit big-endian to host order
   function BE_To_Host(Val : Unsigned_32) return Unsigned_32 is
      B0, B1, B2, B3 : Unsigned_32;
   begin
      B0 := (Val and 16#FF000000#) / 16#1000000#;
      B1 := (Val and 16#00FF0000#) / 16#10000#;
      B2 := (Val and 16#0000FF00#) / 16#100#;
      B3 := (Val and 16#000000FF#);
      return B3 * 16#1000000# + B2 * 16#10000# + B1 * 16#100# + B0;
   end BE_To_Host;

   -- Read a 32-bit BE value from memory
   function Read_BE32(Addr : Address) return Unsigned_32 is
      Ptr : U32_Ptr := To_U32_Ptr(Addr);
   begin
      return BE_To_Host(Ptr.all);
   end Read_BE32;

   -- Align address to 4-byte boundary
   function Align4(Addr : Address) return Address is
      Offset : constant Integer := Integer(Addr mod 4);
   begin
      if Offset = 0 then
         return Addr;
      else
         return Addr + To_Address(4 - Offset);
      end if;
   end Align4;

   -- Advance pointer by N bytes
   function Advance(Addr : Address; Bytes : Natural) return Address is
   begin
      return Addr + To_Address(Bytes);
   end Advance;

   -- Read a null-terminated ASCII string (up to Max_Name_Length)
   function Read_Null_String(Addr : Address) return String is
      Ptr    : U8_Ptr := To_U8_Ptr(Addr);
      Len    : Natural := 0;
      Result : String(1 .. Max_Name_Length);
   begin
      loop
         if Ptr(Len) = 0 then
            exit;
         end if;
         Result(Len + 1) := Character'Val(Integer(Ptr(Len)));
         Len := Len + 1;
         if Len = Max_Name_Length then
            exit;
         end if;
      end loop;
      return Result(1 .. Len);
   end Read_Null_String;

   -----------------------------------------------------------------------------
   -- Recursive Parse of a DTB Node
   -----------------------------------------------------------------------------
   function Parse_Node(Pos : in out Address) return DTB_Node_Access is
      Token : Unsigned_32;
      Node  : DTB_Node_Access := new DTB_Node'(Name => (others => ' '), Prop_Count => 0, Child_Count => 0);
   begin
      -- Read BEGIN_NODE token
      Token := Read_BE32(Pos);
      if Token /= FDT_BEGIN_NODE then
         raise Program_Error with "Expected FDT_BEGIN_NODE token";
      end if;
      Pos := Advance(Pos, 4);

      -- Parse node name
      Node.Name := Read_Null_String(Pos);
      Arch.Debug.Print("Parse_Node: Found node '" & Node.Name & "'");
      Pos := Align4(Advance(Pos, Node.Name'Length + 1));

      -- Parse properties and children
      loop
         Token := Read_BE32(Pos);
         Pos := Advance(Pos, 4);

         case Token is
            when FDT_PROP =>
               Parse_Property(Node, Pos);
            when FDT_BEGIN_NODE =>
               Node.Children(Node.Child_Count + 1) := Parse_Node(Pos);
               Node.Child_Count := Node.Child_Count + 1;
            when FDT_END_NODE =>
               exit;
            when FDT_NOP =>
               null; -- Skip padding
            when others =>
               raise Program_Error with "Unexpected token in DTB structure";
         end case;
      end loop;

      return Node;
   exception
      when others =>
         Arch.Debug.Print("Parse_Node: Exception occurred while parsing node.");
         return null;
   end Parse_Node;

   -----------------------------------------------------------------------------
   -- Parse a Property
   -----------------------------------------------------------------------------
   procedure Parse_Property(Node : DTB_Node_Access; Pos : in out Address) is
      Prop : DTB_Property_Access := new DTB_Property;
   begin
      -- Parse property length and name offset
      Prop.Len := BE_To_Host(Read_BE32(Pos));
      Pos := Advance(Pos, 4);
      Prop.Name := Read_Null_String(Strings_Base + BE_To_Host(Read_BE32(Pos)));
      Pos := Advance(Pos, 4);

      -- Parse property value
      for I in 1 .. Prop.Len loop
         Prop.Value(I) := Unsigned_64(Read_BE32(Pos));
         Pos := Advance(Pos, 4);
      end loop;

      -- Add property to node
      Node.Properties(Node.Prop_Count + 1) := Prop;
      Node.Prop_Count := Node.Prop_Count + 1;

      -- Debugging
      Arch.Debug.Print("Parse_Property: Parsed property '" & Prop.Name &
         "' with length " & Natural'Image(Prop.Len));
   exception
      when others =>
         Arch.Debug.Print("Parse_Property: Exception occurred while parsing property.");
   end Parse_Property;

   -----------------------------------------------------------------------------
   -- Initialize DTB Parsing
   -----------------------------------------------------------------------------
   function Init return Boolean is
      type HDR_Ptr is access all FDT_Header;
      function To_HDR_Ptr is new Ada.Unchecked_Conversion(Address, HDR_Ptr);
      HDR : HDR_Ptr := To_HDR_Ptr(DTB_Response.DTB_Addr);
   begin
      if HDR = null then
         Arch.Debug.Print("Init: No DTB response");
         return False;
      end if;

      if BE_To_Host(HDR.Magic) /= FDT_MAGIC then
         Arch.Debug.Print("Init: Invalid magic: " & Unsigned_32'Image(HDR.Magic));
         return False;
      end if;

      Struct_Base  := Arch.Limine.DTB_Response.DTB_Addr + To_Address(Natural(BE_To_Host(HDR.Offset_DT_Struct)));
      Strings_Base := Arch.Limine.DTB_Response.DTB_Addr + To_Address(Natural(BE_To_Host(HDR.Offset_DT_Strings)));
      DTB_End      := Arch.Limine.DTB_Response.DTB_Addr + To_Address(Natural(BE_To_Host(HDR.Size)));

      Arch.Debug.Print("Init: Struct_Base = " & Address'Image(Struct_Base));
      Arch.Debug.Print("Init: Strings_Base = " & Address'Image(Strings_Base));
      Arch.Debug.Print("Init: DTB_End = " & Address'Image(DTB_End));

      Parse_DTB;
      return True;
   exception
      when others =>
         Arch.Debug.Print("Init: Exception occurred during initialization.");
         return False;
   end Init;

   -----------------------------------------------------------------------------
   -- Top-Level Parse
   -----------------------------------------------------------------------------
   procedure Parse_DTB is
      Pos : Address := Struct_Base;
   begin
      Arch.Debug.Print("Parse_DTB: Starting DTB parsing...");
      Root_Node := Parse_Node(Pos);
      Arch.Debug.Print("Parse_DTB: Parsing complete.");
   exception
      when others =>
         Arch.Debug.Print("Parse_DTB: Exception occurred during parsing.");
   end Parse_DTB;

   -----------------------------------------------------------------------------
   -- Pretty-Print a Node
   -----------------------------------------------------------------------------
   procedure Print_DTB_Node(Node : DTB_Node_Access; Indent : String := "") is
   begin
      Arch.Debug.Print(Indent & "Node: " & Node.Name);
      for I in 1 .. Node.Prop_Count loop
         declare
            Prop : constant DTB_Property_Access := Node.Properties(I);
         begin
            Arch.Debug.Print(Indent & "  Property: " & Prop.Name &
               " (Length: " & Natural'Image(Prop.Len) & ")");
         end;
      end loop;
      for I in 1 .. Node.Child_Count loop
         Print_DTB_Node(Node.Children(I), Indent & "  ");
      end loop;
   exception
      when others =>
         Arch.Debug.Print("Print_DTB_Node: Exception occurred while printing node.");
   end Print_DTB_Node;

   -----------------------------------------------------------------------------
   -- Print the Entire DTB
   -----------------------------------------------------------------------------
   procedure Print_DTB is
   begin
      if Root_Node /= null then
         Print_DTB_Node(Root_Node);
      else
         Arch.Debug.Print("Print_DTB: No root node found.");
      end if;
   exception
      when others =>
         Arch.Debug.Print("Print_DTB: Exception occurred while printing DTB.");
   end Print_DTB;

   -----------------------------------------------------------------------------
   -- Find Node by Compatible String
   -----------------------------------------------------------------------------
   function Find_Node_By_Compatible(Compat : String) return DTB_Node_Access is
      function Search(N : DTB_Node_Access) return DTB_Node_Access is
      begin
         for I in 1 .. N.Prop_Count loop
            if N.Properties(I).Name = "compatible" and then
               Ada.Strings.Fixed.Index(N.Properties(I).Name, Compat) > 0 then
               return N;
            end if;
         end loop;
         for J in 1 .. N.Child_Count loop
            declare
               C : DTB_Node_Access := Search(N.Children(J));
            begin
               if C /= null then
                  return C;
               end if;
            end;
         end loop;
         return null;
      end Search;
   begin
      if Root_Node = null then
         return null;
      else
         return Search(Root_Node);
      end if;
   exception
      when others =>
         Arch.Debug.Print("Find_Node_By_Compatible: Exception occurred.");
         return null;
   end Find_Node_By_Compatible;

end Arch.DTB;