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
   -- DTB Token Constants (per the Flattened Device Tree specification)
   ------------------------------------------------------------------------------
   FDT_BEGIN_NODE : constant Unsigned_32 := 16#1#;
   FDT_END_NODE   : constant Unsigned_32 := 16#2#;
   FDT_PROP       : constant Unsigned_32 := 16#3#;
   FDT_NOP        : constant Unsigned_32 := 16#4#;
   FDT_END        : constant Unsigned_32 := 16#9#;

   ------------------------------------------------------------------------------
   -- Endianness Conversion for 32-bit values
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
   -- BE_To_Host_64: Converts a 64-bit big-endian value to host order.
   --
   -- Splits the 64-bit value into eight bytes and reassembles them in reverse order.
   ------------------------------------------------------------------------------
   function BE_To_Host_64 (Val : Unsigned_64) return Unsigned_64 is
      B0, B1, B2, B3, B4, B5, B6, B7 : Unsigned_64;
   begin
      B0 := (Val and 16#FF00000000000000#) / 16#100000000000000#;
      B1 := (Val and 16#00FF000000000000#) / 16#1000000000000#;
      B2 := (Val and 16#0000FF0000000000#) / 16#10000000000#;
      B3 := (Val and 16#000000FF00000000#) / 16#100000000#;
      B4 := (Val and 16#00000000FF000000#) / 16#1000000#;
      B5 := (Val and 16#0000000000FF0000#) / 16#10000#;
      B6 := (Val and 16#000000000000FF00#) / 16#100#;
      B7 := (Val and 16#00000000000000FF#);
      return B7 * 16#100000000000000# +
             B6 * 16#1000000000000# +
             B5 * 16#10000000000# +
             B4 * 16#100000000# +
             B3 * 16#1000000# +
             B2 * 16#10000# +
             B1 * 16#100# +
             B0;
   end BE_To_Host_64;

   ------------------------------------------------------------------------------
   -- Global Variables for DTB Storage
   ------------------------------------------------------------------------------
   DTB_End      : System.Address := System.Null_Address;
   Root_Node    : DTB_Node_Access := null;
   Struct_Base  : System.Address := System.Null_Address;
   Strings_Base : System.Address := System.Null_Address;

   ------------------------------------------------------------------------------
   -- DTB Header Declaration (fields in the DTB blob are stored in big-endian)
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
      Debug.Print ("Read_UInt32: Bounds verified");
      return UInt32_Ptr(Addr).all;
   end Read_UInt32;

   function Read_BE32 (Addr : System.Address) return Unsigned_32 is
   begin
      Debug.Print ("Read_BE32: " & To_String(Addr));
      return BE_To_Host(Read_UInt32(Addr));
   end Read_BE32;

   ------------------------------------------------------------------------------
   -- Read a Null-Terminated String from Memory.
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
   -- Global Variables for Parsed DTB and Base Addresses
   ------------------------------------------------------------------------------
   Root_Node    : DTB_Node_Access := null;
   Struct_Base  : System.Address := System.Null_Address;
   Strings_Base : System.Address := System.Null_Address;

   ------------------------------------------------------------------------------
   -- DTB Header Parsing
   ------------------------------------------------------------------------------
   function Init return Boolean is
      DTBPonse  : Arch.Limine.DTB_Response
         with Import, Address => Arch.Limine.DTB_Response.Response;
   begin
      Debug.Print ("DTB_Init: Starting DTB parsing");
      if DTBPonse.DTB_Addr = System.Null_Address then
         Arch.Debug.Print("Error: DTB response is null.");
         return False;
      end if;

      -- Convert DTB header fields to host order.
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

      -- Set base addresses from header.
      Struct_Base  := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Offset_DT_Struct));
      Debug.Print ("DTB Struct Base: " & To_String(Struct_Base));
      Strings_Base := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Offset_DT_Strings));
      Debug.Print ("DTB Strings Base: " & To_String(Strings_Base));
      DTB_End      := Advance(DTBPonse.DTB_Addr, To_Integer(Global_Header.Total_Size));
      Debug.Print ("DTB End Address: " & To_String(DTB_End));

      -- Parse the DTB structure and build the in-memory node tree.
      begin
         Debug.Print ("Parse_DTB: Starting DTB parsing");
         Parse_DTB;
         Debug.Print ("Parse_DTB: Finished DTB parsing");
      exception
         when E : others =>
            Arch.Debug.Print("DTB Parsing failed: " & Exception_Message(E));
            return False;
      end;
      Debug.Print ("DTB_Init: Finished DTB parsing");
      return True;
   end Init;

   ------------------------------------------------------------------------------
   -- Recursive Parsing of a DTB Node.
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
      Debug.Print ("Parse_Node: Pos = " & To_String(Pos));
      Token := Read_BE32(Pos);
      Debug.Print ("Parse_Node: Token = " & Unsigned_32'Image(Token));
      if Token /= FDT_BEGIN_NODE then
         Debug.Print("Error: Expected FDT_BEGIN_NODE token at address " & System.Address'Image(Pos));
         raise Program_Error with "Parse_Node: Expected FDT_BEGIN_NODE token";
      end if;
      Pos := Advance(Pos, 4);
      Debug.Print ("Parse_Node: Pos after FDT_BEGIN_NODE = " & To_String(Pos));

      -- Read node name.
      Temp_Str := Read_Null_Terminated_String(Pos);
      New_Node.Name := Temp_Str;
      Debug.Print ("Parse_Node: Node Name = " & To_String(Temp_Str));
      Pos := Align4(Advance(Pos, Natural(Ada.Strings.Unbounded.Length(Temp_Str)) + 1));
      Debug.Print ("Parse_Node: Pos after Node Name = " & To_String(Pos));

      -- Process tokens until FDT_END_NODE.
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
               Debug.Print ("Parse_Node: Property Length = " & Natural'Image(Prop_Length));
               if DTB_Property_Vector.Length(New_Node.Properties) + 1 > Max_Properties_Per_Node then
                  Arch.Debug.Print("Error: Exceeded maximum properties in node " & To_String(New_Node.Name));
                  raise Program_Error with "Exceeded maximum properties in node " & To_String(New_Node.Name);
               end if;
               DTB_Property_Vector.Append(New_Node.Properties,
                  (Name   => Prop_Name,
                   Length => Prop_Length,
                   Value  => Prop_Value));
               Pos := Align4(Advance(Pos, Prop_Length));
            end;
         elsif Token = FDT_BEGIN_NODE then
            if DTB_Node_Vector.Length(New_Node.Children) + 1 > Max_Children_Per_Node then
               Arch.Debug.Print("Error: Exceeded maximum children in node " & To_String(New_Node.Name));
               raise Program_Error with "Exceeded maximum children in node " & To_String(New_Node.Name);
            end if;
            declare
               Child_Node : DTB_Node_Access := Parse_Node(Pos);
            begin
               DTB_Node_Vector.Append(New_Node.Children, Child_Node);
            end;
         elsif Token = FDT_NOP then
            Pos := Advance(Pos, 4);
         elsif Token = FDT_END then
            exit;
         else
            Arch.Debug.Print("Warning: Unknown token encountered at " & System.Address'Image(Pos) & ", skipping 4 bytes.");
            Pos := Advance(Pos, 4);
         end if;
      end loop;
      return New_Node;
   exception
      when E : others =>
         Arch.Debug.Print("Error in Parse_Node: " & Exception_Message(E));
         raise;
   end Parse_Node;

   ------------------------------------------------------------------------------
   -- Parse_DTB: Parses the entire DTB structure block.
   ------------------------------------------------------------------------------
   procedure Parse_DTB is
      Current_Pos : System.Address := Struct_Base;
   begin
      Debug.Print ("Parse_DTB: Starting at Address = " & To_String(Current_Pos));
      Root_Node := Parse_Node(Current_Pos);
   exception
      when E : others =>
         Debug.Print("Error in Parse_DTB: " & Exception_Message(E));
         raise;
   end Parse_DTB;

   ------------------------------------------------------------------------------
   -- Find_Node_By_Compatible:
   --
   -- Recursively searches the parsed DTB tree for a node whose "compatible" property
   -- contains the search string.
   --
   -- Parameters:
   --   Compatible_Search : The string to search for in the "compatible" property.
   --
   -- Returns:
   --   A DTB_Node_Access if a matching node is found; null otherwise.
   ------------------------------------------------------------------------------
   function Find_Node_By_Compatible (Compatible_Search : String) return DTB_Node_Access is
      Compatible_Key : constant Unbounded_String := To_Unbounded_String("compatible");

      -- Recursive helper function.
      function Search (Node : DTB_Node_Access) return DTB_Node_Access is
         Candidate : DTB_Node_Access := null;
         Prop_Value: Unbounded_String;
      begin
         for I in DTB_Property_Vector.First_Index(Node.Properties) ..
                 DTB_Property_Vector.Last_Index(Node.Properties) loop
            if To_String(DTB_Property_Vector.Element(Node.Properties, I).Name) = Compatible_Key then
               Prop_Value := Read_Null_Terminated_String(DTB_Property_Vector.Element(Node.Properties, I).Value);
               if Ada.Strings.Unbounded.Contains(Prop_Value, To_Unbounded_String(Compatible_Search)) then
                  return Node;
               end if;
            end if;
         end loop;
         for J in DTB_Node_Vector.First_Index(Node.Children) ..
                 DTB_Node_Vector.Last_Index(Node.Children) loop
            if Node.Children(J) /= null then
               Candidate := Search(Node.Children(J));
               if Candidate /= null then
                  return Candidate;
               end if;
            end if;
         end loop;
         return null;
      end Search;
   begin
      if Root_Node = null then
         return null;
      else
         return Search(Root_Node);
      end if;
   end Find_Node_By_Compatible;

   ------------------------------------------------------------------------------
   -- Define an unconstrained array type for 64-bit values.
   ------------------------------------------------------------------------------
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;

   ------------------------------------------------------------------------------
   -- Get_Property_Unsigned_64:
   --
   -- Searches the given DTB node for a property named Property_Name and returns its
   -- contents as an array of Unsigned_64 values, converted to host order.
   --
   -- Parameters:
   --   Node          : A DTB_Node_Access representing the node to search.
   --   Property_Name : The property name to look for (e.g., "reg").
   --
   -- Returns:
   --   An array of Unsigned_64 values read from the property, with each cell converted
   --   from big-endian to host order.
   --
   -- Raises:
   --   Program_Error if the property is not found or if its length is not a multiple
   --   of 4 or 8.
   ------------------------------------------------------------------------------
   function Get_Property_Unsigned_64
     (Node          : DTB_Node_Access;
      Property_Name : String)
      return Unsigned_64_Array is
      Found  : Boolean := False;
      Prop   : DTB_Property_Type;
      Index  : Natural;
   begin
      for Index in DTB_Property_Vector.First_Index(Node.Properties) ..
                   DTB_Property_Vector.Last_Index(Node.Properties) loop
         if To_String(DTB_Property_Vector.Element(Node.Properties, Index).Name) = Property_Name then
            Found := True;
            Prop  := DTB_Property_Vector.Element(Node.Properties, Index);
            exit;
         end if;
      end loop;
      if not Found then
         raise Program_Error with "Property " & Property_Name & " not found in node " & To_String(Node.Name);
      end if;

      if Prop.Length mod 8 = 0 then
         declare
            Num_Cells : constant Natural := Prop.Length / 8;
            Result    : Unsigned_64_Array (1 .. Num_Cells) := (others => 0);
         begin
            declare
               type U64_Ptr is access all Unsigned_64;
               Ptr : U64_Ptr := U64_Ptr(Prop.Value);
            begin
               for J in 1 .. Num_Cells loop
                  Result(J) := BE_To_Host_64(Ptr(J));
               end loop;
               return Result;
            end;
         end;
      elsif Prop.Length mod 4 = 0 then
         declare
            Num_Cells : constant Natural := Prop.Length / 4;
            Result    : Unsigned_64_Array (1 .. Num_Cells) := (others => 0);
         begin
            declare
               type U32_Ptr is access all Unsigned_32;
               Ptr : U32_Ptr := U32_Ptr(Prop.Value);
            begin
               for J in 1 .. Num_Cells loop
                  Result(J) := Unsigned_64(BE_To_Host(Ptr(J)));
               end loop;
               return Result;
            end;
         end;
      else
         raise Program_Error with "Property length for " & Property_Name & " is not a multiple of 4 or 8";
      end if;
   end Get_Property_Unsigned_64;

   procedure Print_DTB_Node
  (Node   : DTB_Node_Access;
   Indent : String := "") is
begin
   -- Print the node's name.
   Arch.Debug.Print(Indent & "Node Name: " & To_String(Node.Name));

   -- Print properties.
   if DTB_Property_Vector.Length(Node.Properties) > 0 then
      Arch.Debug.Print(Indent & "  Properties:");
      for I in DTB_Property_Vector.Range(Node.Properties) loop
         declare
            Prop : DTB_Property_Type := DTB_Property_Vector.Element(Node.Properties, I);
         begin
            -- Print property name, length, and address of its value.
            Arch.Debug.Print(Indent & "    " &
                              "Name: " & To_String(Prop.Name) &
                              ", Length: " & Natural'Image(Prop.Length) &
                              ", Value Address: " & System.Address'Image(Prop.Value));
         end;
      end loop;
   else
      Arch.Debug.Print(Indent & "  (No Properties)");
   end if;

   -- Print children.
   if DTB_Node_Vector.Length(Node.Children) > 0 then
      Arch.Debug.Print(Indent & "  Children:");
      for I in DTB_Node_Vector.Range(Node.Children) loop
         declare
            Child : DTB_Node_Access := Node.Children(I);
         begin
            if Child /= null then
               -- Recursively print each child node with increased indent.
               Print_DTB_Node(Child, Indent & "    ");
            else
               Arch.Debug.Print(Indent & "    (Null Child)");
            end if;
         end;
      end loop;
   else
      Arch.Debug.Print(Indent & "  (No Children)");
   end if;
end Print_DTB_Node;
end Arch.DTB;
