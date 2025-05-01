--  arch-dtb.adb: Device-tree blob parsing.
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

with Ada.Containers.Vectors;
with Ada.Strings;            use Ada.Strings;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Exceptions;         use Ada.Exceptions;
with Interfaces;             use Interfaces;
with System;                 use System;
with System.Storage_Elements;use System.Storage_Elements;
with Arch.Limine;
with Arch.Debug;

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
   -- Endianness Conversions
   -----------------------------------------------------------------------------
   function BE_To_Host (Val : Unsigned_32) return Unsigned_32 is
      B0, B1, B2, B3 : Unsigned_32;
   begin
      B0 := (Val and 16#FF000000#) / 16#1000000#;
      B1 := (Val and 16#00FF0000#) / 16#10000#;
      B2 := (Val and 16#0000FF00#) / 16#100#;
      B3 := (Val and 16#000000FF#);
      return B3 * 16#1000000# + B2 * 16#10000# + B1 * 16#100# + B0;
   end BE_To_Host;

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
             B6 * 16#1000000000000#   +
             B5 * 16#10000000000#     +
             B4 * 16#100000000#       +
             B3 * 16#1000000#         +
             B2 * 16#10000#           +
             B1 * 16#100#             +
             B0;
   end BE_To_Host_64;

   -----------------------------------------------------------------------------
   -- Global base pointers
   -----------------------------------------------------------------------------
   DTB_End      : System.Address := System.Null_Address;
   Struct_Base  : System.Address := System.Null_Address;
   Strings_Base : System.Address := System.Null_Address;

   -----------------------------------------------------------------------------
   -- DTB Header
   -----------------------------------------------------------------------------
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

   -----------------------------------------------------------------------------
   -- Bounds checking
   -----------------------------------------------------------------------------
   procedure Check_Bounds (Addr : System.Address; Bytes : Natural) is
   begin
      if Addr + To_Address (Bytes) > DTB_End then
         raise Program_Error with "DTB bounds exceeded";
      end if;
   end Check_Bounds;

   -----------------------------------------------------------------------------
   -- 32-bit & BE reads
   -----------------------------------------------------------------------------
   function Read_UInt32 (Addr : System.Address) return Unsigned_32 is
      type Ptr32 is access all Unsigned_32;
   begin
      Check_Bounds (Addr, 4);
      return Ptr32 (Addr).all;
   end Read_UInt32;

   function Read_BE32 (Addr : System.Address) return Unsigned_32 is
   begin
      return BE_To_Host (Read_UInt32 (Addr));
   end Read_BE32;

   -----------------------------------------------------------------------------
   -- Read a null-terminated string (max 1024 chars)
   -----------------------------------------------------------------------------
   Max_String_Length : constant Positive := 1024;

   function Read_Null_Terminated_String (Addr : System.Address) return String is
      type Local_Str is array (1 .. Max_String_Length) of Character;
      Local_Str_Var : Local_Str := (others => ASCII.NUL);
      Ptr    : access all Unsigned_8 := Addr'To_Access (Unsigned_8);
      Len    : Natural := 0;
      C      : Character;
   begin
      loop
         Check_Bounds (Addr, Len + 1);
         C := Character'Val (Integer (Ptr (Len + 1)));
         exit when C = ASCII.NUL or else Len + 1 > Max_String_Length;
         Len := Len + 1;
         Local_Str (Len) := C;
      end loop;
      return Local_Str (1 .. Len);
   end Read_Null_Terminated_String;

   -----------------------------------------------------------------------------
   -- Pointer arithmetic
   -----------------------------------------------------------------------------
   function Advance (Addr : System.Address; Bytes : Natural) return System.Address is
   begin
      return Addr + To_Address (Bytes);
   end Advance;

   function Align4 (Addr : System.Address) return System.Address is
      Offset : Natural := Natural (Addr mod 4);
   begin
      if Offset = 0 then
         return Addr;
      else
         return Addr + To_Address (4 - Offset);
      end if;
   end Align4;

   -----------------------------------------------------------------------------
   -- Limits
   -----------------------------------------------------------------------------
   Max_Properties_Per_Node : constant Natural := 64;
   Max_Children_Per_Node   : constant Natural := 64;

   -----------------------------------------------------------------------------
   -- Property & Node types
   -----------------------------------------------------------------------------
   type DTB_Property_Type is record
      Name   : String;
      Length : Natural;
      Value  : System.Address;
   end record;

   package DTB_Property_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => DTB_Property_Type);

   type DTB_Node;
   type DTB_Node_Access is access all DTB_Node;

   package DTB_Node_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => DTB_Node_Access);

   type DTB_Node is record
      Name       : String;
      Properties : DTB_Property_Vector.Vector;
      Children   : DTB_Node_Vector.Vector;
   end record;

   Root_Node : DTB_Node_Access := null;

   -----------------------------------------------------------------------------
   -- Initialize & parse
   -----------------------------------------------------------------------------
   function Init return Boolean is
      DTBPonse : Arch.Limine.DTB_Response
        with Import, Address => Arch.Limine.DTB_Response.Response;
   begin
      if DTBPonse.DTB_Addr = System.Null_Address then
         return False;
      end if;

      Global_Header.Magic := Read_BE32 (DTBPonse.DTB_Addr);
      if Global_Header.Magic /= 16#D00DFEED# then
         return False;
      end if;

      Struct_Base  := Advance (DTBPonse.DTB_Addr, Integer (Global_Header.Offset_DT_Struct));
      Strings_Base := Advance (DTBPonse.DTB_Addr, Integer (Global_Header.Offset_DT_Strings));
      DTB_End      := Advance (DTBPonse.DTB_Addr, Integer (Global_Header.Total_Size));

      begin
         Parse_DTB;
      exception
         when E : others =>
            return False;
      end;

      return True;
   end Init;

   -----------------------------------------------------------------------------
   -- Recursive node parsing
   -----------------------------------------------------------------------------
   function Parse_Node (Pos : in out System.Address) return DTB_Node_Access is
      New_Node : DTB_Node_Access := new DTB_Node'(
         Name       => "",
         Properties => DTB_Property_Vector.Empty_Vector,
         Children   => DTB_Node_Vector.Empty_Vector
      );
      Token    : Unsigned_32;
   begin
      Token := Read_BE32 (Pos);
      if Token /= FDT_BEGIN_NODE then
         raise Program_Error with "Expected FDT_BEGIN_NODE";
      end if;
      Pos := Advance (Pos, 4);

      -- Read node name
      declare
         Name_Str : String := Read_Null_Terminated_String (Pos);
      begin
         New_Node.Name := Name_Str;
         Pos := Align4 (Advance (Pos, Length (Name_Str) + 1));
      end;

      -- Parse children & properties
      loop
         Token := Read_BE32 (Pos);
         if Token = FDT_END_NODE then
            Pos := Advance (Pos, 4);
            exit;
         elsif Token = FDT_PROP then
            Pos := Advance (Pos, 4);
            declare
               Prop_Length : Natural := Natural (Read_BE32 (Pos));
               Name_Offset : Natural := Natural (Read_BE32 (Advance (Pos, 4)));
               Prop_Name   : String  := Read_Null_Terminated_String (Strings_Base + To_Address (Name_Offset));
               Prop_Value  : System.Address := Pos;
            begin
               DTB_Property_Vector.Append (
                  New_Node.Properties,
                  ( Name   => Prop_Name,
                    Length => Prop_Length,
                    Value  => Prop_Value
                  )
               );
               Pos := Align4 (Advance (Pos, Prop_Length));
            end;
         elsif Token = FDT_BEGIN_NODE then
            declare
               Child : DTB_Node_Access := Parse_Node (Pos);
            begin
               DTB_Node_Vector.Append (New_Node.Children, Child);
            end;
         elsif Token = FDT_NOP then
            Pos := Advance (Pos, 4);
         elsif Token = FDT_END then
            exit;
         else
            Pos := Advance (Pos, 4);
         end if;
      end loop;

      return New_Node;
   end Parse_Node;

   -----------------------------------------------------------------------------
   -- Kick off parsing
   -----------------------------------------------------------------------------
   procedure Parse_DTB is
      Current_Pos : System.Address := Struct_Base;
   begin
      Root_Node := Parse_Node (Current_Pos);
   end Parse_DTB;

   -----------------------------------------------------------------------------
   -- Fetch 64-bit properties
   -----------------------------------------------------------------------------
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;

   function Get_Property_Unsigned_64
     (Node          : DTB_Node_Access;
      Property_Name : String)
      return Unsigned_64_Array is
      Found  : Boolean := False;
      Prop   : DTB_Property_Type;
   begin
      for I in DTB_Property_Vector.First_Index (Node.Properties) ..
               DTB_Property_Vector.Last_Index  (Node.Properties) loop
         if Node.Properties (I).Name = Property_Name then
            Found := True;
            Prop  := Node.Properties (I);
            exit;
         end if;
      end loop;

      if not Found then
         raise Program_Error with "Property not found: " & Property_Name;
      end if;

      if Prop.Length mod 8 = 0 then
         declare
            Num_Cells : constant Natural := Prop.Length / 8;
            type U64_Ptr is access all Unsigned_64;
            Ptr        : U64_Ptr := U64_Ptr (Prop.Value);
            Result     : Unsigned_64_Array (1 .. Num_Cells);
         begin
            for J in 1 .. Num_Cells loop
               Result (J) := BE_To_Host_64 (Ptr (J));
            end loop;
            return Result;
         end;
      elsif Prop.Length mod 4 = 0 then
         declare
            Num_Cells : constant Natural := Prop.Length / 4;
            type U32_Ptr is access all Unsigned_32;
            Ptr        : U32_Ptr := U32_Ptr (Prop.Value);
            Result     : Unsigned_64_Array (1 .. Num_Cells);
         begin
            for J in 1 .. Num_Cells loop
               Result (J) := Unsigned_64 (BE_To_Host (Ptr (J)));
            end loop;
            return Result;
         end;
      else
         raise Program_Error with "Invalid property length for " & Property_Name;
      end if;
   end Get_Property_Unsigned_64;

   -----------------------------------------------------------------------------
   -- Read a fixed-length block as a String
   -----------------------------------------------------------------------------
   function Property_Value_To_String
     (Value  : System.Address;
      Length : Natural)
     return String is
      Max_Buf : constant Positive := 1024;
      type Local_Str is array (1 .. Max_Buf) of Character;
      Ptr       : access all Unsigned_8 := Value'To_Access (Unsigned_8);
   begin
      if Length > Max_Buf then
         raise Program_Error with "Value_To_String length too large";
      end if;
      for I in 1 .. Length loop
         Local_Str (I) := Character'Val (Integer (Ptr (I)));
      end loop;
      return Local_Str (1 .. Length);
   end Property_Value_To_String;

   -----------------------------------------------------------------------------
   -- Pretty-print the DTB tree
   -----------------------------------------------------------------------------
   procedure Print_DTB_Node
     (Node   : DTB_Node_Access;
      Indent : String := "") is
   begin
      Arch.Debug.Print (Indent & "Node: " & Node.Name);

      Arch.Debug.Print (Indent & "  Properties:");
      for I in DTB_Property_Vector.First_Index (Node.Properties) ..
               DTB_Property_Vector.Last_Index  (Node.Properties) loop
         declare
            P       : DTB_Property_Type := Node.Properties (I);
            Val_Str : String            := Property_Value_To_String (P.Value, P.Length);
         begin
            Arch.Debug.Print (
              Indent & "    " &
              "Name: "       & P.Name   & ", " &
              "Len: "        & Natural'Image (P.Length) & ", " &
              "Addr: "       & System.Address'Image (P.Value)
            );
            Arch.Debug.Print (
              Indent & "      Value: " & Val_Str
            );
         end;
      end loop;

      Arch.Debug.Print (Indent & "  Children:");
      for J in DTB_Node_Vector.First_Index (Node.Children) ..
               DTB_Node_Vector.Last_Index  (Node.Children) loop
         if Node.Children (J) /= null then
            Print_DTB_Node (Node.Children (J), Indent & "    ");
         end if;
      end loop;
   end Print_DTB_Node;

end Arch.DTB;
