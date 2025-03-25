--  arch-dtb.adb: Device-tree blob parsing.
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Exceptions;                  use Ada.Exceptions;
with Interfaces;                      use Interfaces;
with System;                          use System;
with System.Storage_Elements;         use System.Storage_Elements;
with Arch.Limine;
with Arch.Debug;                      -- for debug and error logging
with Ada.Strings.Unbounded.Text_IO;

package body Arch.DTB with SPARK_Mode => Off is

   ------------------------------------------------------------------------------
   -- DTB Token Constants (Flattened Device Tree spec)
   ------------------------------------------------------------------------------
   FDT_BEGIN_NODE : constant Unsigned_32 := 16#1#;
   FDT_END_NODE   : constant Unsigned_32 := 16#2#;
   FDT_PROP       : constant Unsigned_32 := 16#3#;
   FDT_NOP        : constant Unsigned_32 := 16#4#;
   FDT_END        : constant Unsigned_32 := 16#9#;

   ------------------------------------------------------------------------------
   -- Endianness Conversion
   -- The DTB is stored in big-endian format; convert a 32-bit value to host order.
   ------------------------------------------------------------------------------
   function BE_To_Host (Val : Unsigned_32) return Unsigned_32 is
      B0, B1, B2, B3 : Unsigned_32;
   begin
      B0 := (Val and 16#FF000000#) / 16#1000000#;
      B1 := (Val and 16#00FF0000#) / 16#10000#;
      B2 := (Val and 16#0000FF00#) / 16#100#;
      B3 := (Val and 16#000000FF#);
      Debug.Print ("BE_To_Host: " & Unsigned_32'Image(Val) & " -> " &
                   Unsigned_32'Image(B3 * 16#1000000#) & Unsigned_32'Image(B2 * 16#10000#) &
                   Unsigned_32'Image(B1 * 16#100#) & Unsigned_32'Image(B0));
      return B3 * 16#1000000# + B2 * 16#10000# + B1 * 16#100# + B0;
   end BE_To_Host;

   ------------------------------------------------------------------------------
   -- Global variable to store the end address of the DTB blob (for bounds checking)
   ------------------------------------------------------------------------------
   DTB_End : System.Address := System.Null_Address;

   ------------------------------------------------------------------------------
   -- Global header in host order (converted once during Init).
   ------------------------------------------------------------------------------
   Global_Header : FDT_Header;

   ------------------------------------------------------------------------------
   -- Memory Reading Helpers with Bounds Checking
   ------------------------------------------------------------------------------
   procedure Check_Bounds (Addr : System.Address; Bytes : Natural) is
   begin
      Debug.Print ("Check_Bounds: " & To_String(Addr) & " + " & To_String(Bytes));
      if Addr + To_Address(Bytes) > DTB_End then
         Debug.Print("Error: Attempted to read beyond DTB bounds");
         raise Program_Error with "DTB bounds exceeded";
      end if;
   end Check_Bounds;

   function Read_UInt32 (Addr : System.Address) return Unsigned_32 is
      type UInt32_Ptr is access all Unsigned_32;
   begin
      Debug.Print ("Read_UInt32: " & To_String(Addr));
      Check_Bounds(Addr, 4);
      Debug.Print ("Read_UInt32 Successfully Checked Bounds");
      return UInt32_Ptr(Addr).all;
   end Read_UInt32;

   function Read_BE32 (Addr : System.Address) return Unsigned_32 is
   begin
      Debug.Print ("Read_BE32: " & To_String(Addr));
      return BE_To_Host(Read_UInt32(Addr));
   end Read_BE32;

   ------------------------------------------------------------------------------
   -- Read a Null-Terminated String from Memory
   -- Returns an Unbounded_String.
   ------------------------------------------------------------------------------
   function Read_Null_Terminated_String (Addr : System.Address) return Unbounded_String is
      type Byte_Ptr is access all Unsigned_8;
      Ptr    : constant Byte_Ptr := Addr'To_Access(Byte_Ptr);
      Result : Unbounded_String := To_Unbounded_String("");
      C      : Character;
      Index  : Natural := 0;
   begin
      loop
         Index := Index + 1;
         Check_Bounds(Addr, Index);
         C := Character'Val(Integer(Ptr(Index)));
         exit when C = ASCII.NUL;
         Result := Result & To_Unbounded_String(C);
         Debug.Print ("Read_Null_Terminated_String: " & To_String(Result));
         if Index > 1024 then  -- safeguard limit
            Arch.Debug.Print("Error: String length exceeded maximum limit");
            raise Program_Error with "String length exceeded maximum limit";
         end if;
      end loop;
      Debug.Print ("Final Read_Null_Terminated_String: " & To_String(Result));
      return Result;
   end Read_Null_Terminated_String;

   ------------------------------------------------------------------------------
   -- Pointer Arithmetic Helpers
   ------------------------------------------------------------------------------
   function Advance (Addr : System.Address; Bytes : Natural) return System.Address is
   begin
      Debug.Print ("Advance: " & To_String(Addr) & " + " & To_String(Bytes));
      return Addr + To_Address(Bytes);
   end Advance;

   function Align4 (Addr : System.Address) return System.Address is
      Offset : constant Natural := Natural(Addr mod 4);
   begin
      if Offset = 0 then
         Debug.Print ("Align4: " & To_String(Addr));
         return Addr;
      else
         Debug.Print ("Align4: " & To_String(Addr) & " + " & To_String(4 - Offset));
         return Addr + To_Address(4 - Offset);
      end if;
   end Align4;

   ------------------------------------------------------------------------------
   -- Memory Pool Limits for DTB Parsing
   ------------------------------------------------------------------------------
   Max_Properties_Per_Node : constant Natural := 64;
   Max_Children_Per_Node   : constant Natural := 64;

   ------------------------------------------------------------------------------
   -- Dynamic Containers for DTB Properties and Nodes
   ------------------------------------------------------------------------------
   type DTB_Property_Type is record
      Name   : Unbounded_String;
      Length : Natural;
      Value  : System.Address;  -- Pointer to the property value in the DTB blob.
   end record;

   package DTB_Property_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => DTB_Property_Type);

   type DTB_Node;
   type DTB_Node_Access is access all DTB_Node;

   package DTB_Node_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => DTB_Node_Access);

   type DTB_Node is record
      Name       : Unbounded_String;
      Properties : DTB_Property_Vector.Vector;
      Children   : DTB_Node_Vector.Vector;
   end record;

   ------------------------------------------------------------------------------
   -- Global Variables for Parsed DTB and Block Base Addresses
   ------------------------------------------------------------------------------
   Root_Node    : DTB_Node_Access := null;
   Struct_Base  : System.Address := System.Null_Address;
   Strings_Base : System.Address := System.Null_Address;

   ------------------------------------------------------------------------------
   -- DTB Header Declaration (fields in DTB blob are big-endian)
   ------------------------------------------------------------------------------
   type FDT_Header is record
      Magic                : Unsigned_32;
      Total_Size           : Unsigned_32;
      Offset_DT_Struct     : Unsigned_32;
      Offset_DT_Strings    : Unsigned_32;
      Offset_Reserved      : Unsigned_32;
      Version              : Unsigned_32;
      Last_Compatible_Vers : Unsigned_32;
      Boot_CPU_Physical_ID : Unsigned_32;
      Size_DT_Strings      : Unsigned_32;
      Size_DT_Struct       : Unsigned_32;
   end record;
   for FDT_Header'Alignment use 4;

   ------------------------------------------------------------------------------
   -- Recursive Parsing of a DTB Node
   -- Parses a node from the structure block starting at Pos, allocates a new DTB_Node,
   -- and advances Pos accordingly.
   ------------------------------------------------------------------------------
   function Parse_Node (Pos : in out System.Address) return DTB_Node_Access is
      New_Node : DTB_Node_Access :=
         new DTB_Node'( Name       => To_Unbounded_String(""),
                        Properties => DTB_Property_Vector.Empty_Vector,
                        Children   => DTB_Node_Vector.Empty_Vector );
      Token    : Unsigned_32;
      Temp_Str : Unbounded_String;
   begin
      -- Verify and consume the FDT_BEGIN_NODE token.
      Debug.Print ("Parse_Node: Pos = " & To_String(Pos));
      Token := Read_BE32(Pos);
      Debug.Print ("Parse_Node: Token = " & Unsigned_32'Image(Token));
      if Token /= FDT_BEGIN_NODE then
         Debug.Print("Error: Expected FDT_BEGIN_NODE token at address " & System.Address'Image(Pos));
         raise Program_Error with "Parse_Node: Expected FDT_BEGIN_NODE token";
      end if;
      Pos := Advance(Pos, 4);
      Debug.Print ("Parse_Node: Pos after FDT_BEGIN_NODE = " & To_String(Pos));

      -- Read the node name (null-terminated string) and store it.
      Temp_Str := Read_Null_Terminated_String(Pos);
      New_Node.Name := Temp_Str;
      Debug.Print ("Parse_Node: Node Name = " & To_String(Temp_Str));
      Pos := Align4(Advance(Pos, Natural(Ada.Strings.Unbounded.Length(Temp_Str)) + 1));
      Debug.Print ("Parse_Node: Pos after Node Name = " & To_String(Pos));

      -- Process tokens until FDT_END_NODE is encountered.
      loop
         Token := Read_BE32(Pos);
         Debug.Print ("Parse_Node: Token = " & Unsigned_32'Image(Token));
         if Token = FDT_END_NODE then
            Pos := Advance(Pos, 4);
            Debug.Print ("Parse_Node: Pos after FDT_END_NODE = " & To_String(Pos));
            exit;
         elsif Token = FDT_PROP then
            Pos := Advance(Pos, 4);
            Debug.Print ("Parse_Node: Pos after FDT_PROP = " & To_String(Pos));
            declare
               Prop_Length : Natural := Natural(Read_BE32(Pos));
               Name_Offset : Natural := Natural(Read_BE32(Advance(Pos, 4)));
               Prop_Name   : Unbounded_String :=
                               Read_Null_Terminated_String(Strings_Base + To_Address(Name_Offset));
               Prop_Value  : System.Address := Pos;
            begin
               Debug.Print ("Parse_Node: Property Name = " & To_String(Prop_Name));
               Debug.Print ("Parse_Node: Property Value = " & To_String(Prop_Value));
               Debug.Print ("Parse_Node: Property Length = " & Natural'Image(Prop_Length));
               Debug.Print ("Parse_Node: Name_Offset = " & Natural'Image(Name_Offset));
               if DTB_Property_Vector.Length(New_Node.Properties) + 1 > Max_Properties_Per_Node then
                  Arch.Debug.Print("Error: Exceeded maximum properties in node " & To_String(New_Node.Name));
                  raise Program_Error with "Exceeded maximum properties in node " & To_String(New_Node.Name);
               end if;
               Debug.Print ("Parse_Node: Appending Property " & To_String(Prop_Name));
               DTB_Property_Vector.Append(New_Node.Properties,
                  (Name   => Prop_Name,
                   Length => Prop_Length,
                   Value  => Prop_Value));
               Debug.Print ("Parse_Node: Appended Property " & To_String(Prop_Name));
               Pos := Align4(Advance(Pos, Prop_Length));
               Debug.Print ("Parse_Node: Pos after Property = " & To_String(Pos));
            end;
         elsif Token = FDT_BEGIN_NODE then
            Debug.Print ("Parse_Node: Recursing into child node");
            if DTB_Node_Vector.Length(New_Node.Children) + 1 > Max_Children_Per_Node then
               Arch.Debug.Print("Error: Exceeded maximum children in node " & To_String(New_Node.Name));
               raise Program_Error with "Exceeded maximum children in node " & To_String(New_Node.Name);
            end if;
            declare
               Child_Node : DTB_Node_Access := Parse_Node(Pos);
            begin
               Debug.Print ("Parse_Node: Appending Child Node " & To_String(Child_Node.Name));
               DTB_Node_Vector.Append(New_Node.Children, Child_Node);
            end;
         elsif Token = FDT_NOP then
            Debug.Print ("Parse_Node: Skipping NOP token at Pos = " & To_String(Pos));
            Pos := Advance(Pos, 4);
         elsif Token = FDT_END then
            Debug.Print ("Parse_Node: End of DTB reached");
            exit;
         else
            Arch.Debug.Print("Warning: Unknown token encountered at " & System.Address'Image(Pos) & ", skipping 4 bytes.");
            Pos := Advance(Pos, 4);
            Debug.Print ("Parse_Node: Pos after Unknown = " & To_String(Pos));
         end if;
         Debug.Print ("Parse_Node: Looping back to check next token at Pos = " & To_String(Pos));
      end loop;
      Debug.Print ("Parse_Node: Returning Node " & To_String(New_Node.Name));
      return New_Node;
   exception
      when E : others =>
         Arch.Debug.Print("Error in Parse_Node: " & Exception_Message(E));
         raise;
   end Parse_Node;

   ------------------------------------------------------------------------------
   -- Parse the Entire DTB Structure Block
   ------------------------------------------------------------------------------
   procedure Parse_DTB is
      Current_Pos : System.Address := Struct_Base;
   begin
      Debug.Print ("Parse_DTB: Starting at Root_Node Address = " & To_String(Current_Pos));
      Root_Node := Parse_Node(Current_Pos);
   exception
      when E : others =>
         Debug.Print("Error in Parse_DTB: " & Exception_Message(E));
         raise;
   end Parse_DTB;

   ------------------------------------------------------------------------------
   -- Initialization Function
   -- Verifies the DTB header (via Limine), converts header fields once from big-endian,
   -- stores them in Global_Header, computes base addresses, DTB bounds, and parses the DTB.
   ------------------------------------------------------------------------------
   function Init return Boolean is
      DTBPonse  : Arch.Limine.DTB_Response
         with Import, Address => DTB_Request.Response;
   begin
      Debug.Print ("DTB_Init: Starting DTB parsing");
      if DTB_Request.Response = System.Null_Address then
         Arch.Debug.Print("Error: DTB response is null.");
         return False;
      end if;

      -- Read and convert DTB header fields once.
      Global_Header.Magic                := Read_BE32(DTBPonse.DTB_Addr);
      Debug.Print ("DTB Magic: " & Unsigned_32'Image(Global_Header.Magic));
      Global_Header.Total_Size           := Read_BE32(Advance(DTBPonse.DTB_Addr, 4));
      Debug.Print ("DTB Total Size: " & Unsigned_32'Image(Global_Header.Total_Size));
      Global_Header.Offset_DT_Struct     := Read_BE32(Advance(DTBPonse.DTB_Addr, 8));
      Debug.Print ("DTB Struct Offset: " & Unsigned_32'Image(Global_Header.Offset_DT_Struct));
      Global_Header.Offset_DT_Strings    := Read_BE32(Advance(DTBPonse.DTB_Addr, 12));
      Debug.Print ("DTB Strings Offset: " & Unsigned_32'Image(Global_Header.Offset_DT_Strings));
      Global_Header.Offset_Reserved      := Read_BE32(Advance(DTBPonse.DTB_Addr, 16));
      Debug.Print ("DTB Reserved Offset: " & Unsigned_32'Image(Global_Header.Offset_Reserved));
      Global_Header.Version              := Read_BE32(Advance(DTBPonse.DTB_Addr, 20));
      Debug.Print ("DTB Version: " & Unsigned_32'Image(Global_Header.Version));
      Global_Header.Last_Compatible_Vers := Read_BE32(Advance(DTBPonse.DTB_Addr, 24));
      Debug.Print ("DTB Last Compatible Version: " & Unsigned_32'Image(Global_Header.Last_Compatible_Vers));
      Global_Header.Boot_CPU_Physical_ID := Read_BE32(Advance(DTBPonse.DTB_Addr, 28));
      Debug.Print ("DTB Boot CPU Physical ID: " & Unsigned_32'Image(Global_Header.Boot_CPU_Physical_ID));
      Global_Header.Size_DT_Strings      := Read_BE32(Advance(DTBPonse.DTB_Addr, 32));
      Debug.Print ("DTB Size of Strings: " & Unsigned_32'Image(Global_Header.Size_DT_Strings));
      Global_Header.Size_DT_Struct       := Read_BE32(Advance(DTBPonse.DTB_Addr, 36));
      Debug.Print ("DTB Size of Struct: " & Unsigned_32'Image(Global_Header.Size_DT_Struct));

      if Global_Header.Magic /= 16#D00DFEED# then
         Arch.Debug.Print("Error: Invalid DTB magic number: " & Unsigned_32'Image(Global_Header.Magic));
         return False;
      end if;

      -- Set base addresses using the stored header values.
      Struct_Base  := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Offset_DT_Struct));
      Debug.Print ("DTB Struct Base: " & To_String(Struct_Base) & " = DTB.Addr()" & To_String(DTBPonse.DTB_Addr) 
         & ") + DTB Struct Offset()" & To_String(Global_Header.Offset_DT_Struct) & ").");
      Strings_Base := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Offset_DT_Strings));
      Debug.Print ("DTB Strings Base: " & To_String(Strings_Base) & " = DTB.Addr(" & To_String(DTBPonse.DTB_Addr) 
         & ") + DTB Strings Offset(" & To_String(Global_Header.Offset_DT_Strings) & ").");
      DTB_End      := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Total_Size));
      Debug.Print ("DTB End Address: " & To_String(DTB_End) & " = DTB.Addr(" & To_String(DTBPonse.DTB_Addr) 
         & ") + DTB Total Size(" & To_String(Global_Header.Total_Size) & ").");

      begin
         Debug.Print ("Parse_DTB: Starting DTB parsing");
         Parse_DTB;
         Debug.Print ("Parse_DTB: Finished DTB parsing");
      exception
         when E : others =>
            Arch.Debug.Print("DTB Parsing failed: " & Exception_Message(E));
            Debug.Print ("Init: Returning False");
            return False;
      end;
      Debug.Print ("DTB_Init: Finished DTB parsing");
      Debug.Print ("Init: Returning True");
      return True;
   end Init;

   ------------------------------------------------------------------------------
   -- Query Function: Find a Node by its "compatible" Property
   -- Recursively searches the parsed DTB tree for a node whose "compatible"
   -- property contains the search string.
   ------------------------------------------------------------------------------
   function Find_Node_By_Compatible (Compatible_Search : String) return DTB_Node_Access is
      Compatible_Key : constant Unbounded_String := To_Unbounded_String("compatible");

      -- Recursive helper function.
      function Search (Node : DTB_Node_Access) return DTB_Node_Access is
         Candidate : DTB_Node_Access := null;
         Prop_Value: Unbounded_String;
      begin
         Debug.Print ("Find_Node_By_Compatible: Searching Node " & To_String(Node.Name));
         for I in DTB_Property_Vector.First_Index(Node.Properties) ..
                 DTB_Property_Vector.Last_Index(Node.Properties) loop
            Debug.Print ("Find_Node_By_Compatible: Checking Property " & To_String(DTB_Property_Vector.Element(Node.Properties, I).Name));
            if DTB_Property_Vector.Element(Node.Properties, I).Name = Compatible_Key then
               Prop_Value := Read_Null_Terminated_String(DTB_Property_Vector.Element(Node.Properties, I).Value);
               if Ada.Strings.Unbounded.Contains(Prop_Value, To_Unbounded_String(Compatible_Search)) then
                  Debug.Print ("Find_Node_By_Compatible: Found Node " & To_String(Node.Name));
                  Debug.Print ("Find_Node_By_Compatible: Compatible Property Value = " & To_String(Prop_Value));
                  Debug.Print ("Find_Node_By_Compatible: Returning Node.Name = " & To_String(Node.Name));
                  return Node;
               end if;
            end if;
         end loop;
         for J in DTB_Node_Vector.First_Index(Node.Children) ..
                 DTB_Node_Vector.Last_Index(Node.Children) loop
            Debug.Print ("Find_Node_By_Compatible: Recursing into Child Node " & To_String(DTB_Node_Vector.Element(Node.Children, J).Name));
            if Node.Children(J) /= null then
               Debug.Print ("Find_Node_By_Compatible: Searching Child Node " & To_String(DTB_Node_Vector.Element(Node.Children, J).Name));
               Candidate := Search(Node.Children(J));
               if Candidate /= null then
                  Debug.Print ("Find_Node_By_Compatible: Returning Candidate = " & To_String(Candidate.Name));
                  return Candidate;
               end if;
            end if;
         end loop;
         return null;
      end Search;
   begin
      if Root_Node = null then
         Debug.Print ("Find_Node_By_Compatible: Root_Node is null, returning null");
         return null;
      else
         Debug.Print ("Find_Node_By_Compatible: Searching for Compatible = " & Compatible_Search);
         return Search(Root_Node);
      end if;
   end Find_Node_By_Compatible;

end Arch.DTB;
