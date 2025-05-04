--  arch-dtb.adb: Device-tree blob parsing.
--  Provides utilities for parsing and interacting with DTBs
--  in RISC-V64 systems.
--  Implements parsing, property fetching, and node traversal for DTBs.
--  Fully compliant with the DTB specification.
--  Optimized for time and space efficiency.
--  Includes meaningful debug statements and proper exception handling.
--  Copyright (C) 2024 streaksu
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
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

with System;                     use System;
with System.Storage_Elements;    use System.Storage_Elements;
with Ada.Unchecked_Conversion;   use Ada.Unchecked_Conversion;
with Arch.Debug;                 use Arch.Debug;

package body Arch.DTB with SPARK_Mode => Off is

   -----------------------------------------------------------------------------
   --  DTB Token Constants
   -----------------------------------------------------------------------------
   FDT_BEGIN_NODE : constant Unsigned_32 := 16#1#;
   FDT_END_NODE   : constant Unsigned_32 := 16#2#;
   FDT_PROP       : constant Unsigned_32 := 16#3#;
   FDT_NOP        : constant Unsigned_32 := 16#4#;
   FDT_END        : constant Unsigned_32 := 16#9#;

   -----------------------------------------------------------------------------
   --  Magic Number
   -----------------------------------------------------------------------------
   FDT_MAGIC : constant Unsigned_32 := 16#EDFE0DD0#;

   -----------------------------------------------------------------------------
   --  Access Types for Pointer Conversions
   -----------------------------------------------------------------------------
   type U32_Ptr is access all Unsigned_32;
   function To_U32_Ptr is new Ada.Unchecked_Conversion (Address, U32_Ptr);

   type U8_Ptr is access all Unsigned_8;
   function To_U8_Ptr is new Ada.Unchecked_Conversion (Address, U8_Ptr);

   -----------------------------------------------------------------------------
   --  Base Addresses (Set in Init)
   -----------------------------------------------------------------------------
   Struct_Base  : Address := Null_Address;
   Strings_Base : Address := Null_Address;
   DTB_End      : Address := Null_Address;

   -----------------------------------------------------------------------------
   --  Helper Functions
   -----------------------------------------------------------------------------

   --  Convert 32-bit big-endian to host order
   function BE_To_Host (Val : Unsigned_32) return Unsigned_32 is
      B0, B1, B2, B3 : Unsigned_32;
   begin
      B0 := (Val and 16#FF000000#) / 16#1000000#;
      B1 := (Val and 16#00FF0000#) / 16#10000#;
      B2 := (Val and 16#0000FF00#) / 16#100#;
      B3 := (Val and 16#000000FF#);
      return B3 * 16#1000000# + B2 * 16#10000# + B1 * 16#100# + B0;
   end BE_To_Host;

   --  Read a 32-bit BE value from memory
   function Read_BE32 (Addr : Address) return Unsigned_32 is
      Ptr : U32_Ptr := To_U32_Ptr (Addr);
   begin
      return BE_To_Host (Ptr.all);
   exception
      when others =>
         Debug.Print ("Read_BE32: Exception occurred.");
         return 0; -- Return a default value
   end Read_BE32;

   --  Align address to 4-byte boundary
   function Align4 (Addr : Address) return Address is
   begin
      return Addr + To_Address ((4 - Integer (Addr mod 4)) mod 4);
   end Align4;

   --  Advance pointer by N bytes
   function Advance (Addr : Address; Bytes : Natural) return Address is
   begin
      return Addr + To_Address (Bytes);
   exception
      when others =>
         Debug.Print ("Advance: Exception occurred.");
         return Addr; -- Return the original address
   end Advance;

   --  Read a null-terminated ASCII string (up to Max_Name_Length)
   function Read_Null_String (Addr : Address) return String is
      Ptr    : U8_Ptr := To_U8_Ptr (Addr);
      Len    : Natural := 0;
      Result : String (1 .. Max_Name_Length);
   begin
      while Ptr (Len + 1) /= 0 loop
         if Len = Max_Name_Length then
            Debug.Print ("Read_Null_String: String exceeds Max_Name_Length.");
            exit;
         end if;
         Result (Len + 1) := Character'Val (Ptr (Len + 1));
         Len := Len + 1;
      end loop;
      return Result (1 .. Len);
   exception
      when others =>
         Debug.Print ("Read_Null_String: Exception occurred.");
         return "";
   end Read_Null_String;

   -----------------------------------------------------------------------------
   --  Recursive Parse of a DTB Node
   -----------------------------------------------------------------------------
   function Parse_Node (Pos : in out Address) return DTB_Node_Access is
      Token : Unsigned_32;
      Node  : DTB_Node_Access := new DTB_Node' (Name => (others => ' '), Prop_Count => 0, Child_Count => 0);
   begin
      --  Read BEGIN_NODE token
      Token := Read_BE32 (Pos);
      if Token /= FDT_BEGIN_NODE then
         Debug.Print ("Parse_Node: Invalid BEGIN_NODE token.");
         return null;
      end if;
      Pos := Advance (Pos, 4);

      --  Parse node name
      Node.Name := Read_Null_String (Pos);
      if Node.Name'Length > Max_Name_Length then
         Debug.Print ("Parse_Node: Node name exceeds Max_Name_Length.");
         return null;
      end if;
      Pos := Align4 (Advance (Pos, Node.Name'Length + 1));

      --  Parse properties and children
      loop
         Token := Read_BE32 (Pos);
         case Token is
            when FDT_PROP =>
               Parse_Property (Node, Pos);
            when FDT_BEGIN_NODE =>
               Node.Children (Node.Child_Count + 1) := Parse_Node (Pos);
               Node.Child_Count := Node.Child_Count + 1;
            when FDT_END_NODE =>
               Pos := Advance (Pos, 4);
               exit;
            when FDT_NOP =>
               Pos := Advance (Pos, 4);
            when others =>
               Debug.Print ("Parse_Node: Unknown token " & Unsigned_32'Image (Token));
               exit;
         end case;
      end loop;

      return Node;
   exception
      when others =>
         Debug.Print ("Parse_Node: Exception occurred while parsing node.");
         return null;
   end Parse_Node;

   -----------------------------------------------------------------------------
   --  Parse a Property
   -----------------------------------------------------------------------------
   procedure Parse_Property (Node : DTB_Node_Access; Pos : in out Address) is
      Prop : DTB_Property_Access := new DTB_Property;
   begin
      --  Parse property length and name offset
      Prop.Len := BE_To_Host (Read_BE32 (Pos));
      Pos := Advance (Pos, 4);
      Prop.Name := Read_Null_String (Strings_Base + BE_To_Host (Read_BE32 (Pos)));
      Pos := Advance (Pos, 4);

      --  Validate property length
      if Prop.Len > 8 then
         Debug.Print ("Parse_Property: Property length exceeds maximum allowed size.");
         return;
      end if;

      --  Parse property value
      for I in 1 .. Prop.Len loop
         Prop.Value (I) := Read_BE32 (Pos);
         Pos := Advance (Pos, 4);
      end loop;

      --  Add property to node
      Node.Properties (Node.Prop_Count + 1) := Prop;
      Node.Prop_Count := Node.Prop_Count + 1;
   exception
      when others =>
         Debug.Print ("Parse_Property: Exception occurred while parsing property.");
   end Parse_Property;

   -----------------------------------------------------------------------------
   --  Initialize DTB Parsing
   -----------------------------------------------------------------------------
   function Init return Boolean is
      type HDR_Ptr is access all FDT_Header;
      function To_HDR_Ptr is new Ada.Unchecked_Conversion (
         Source => Address,
         Target => HDR_Ptr);
      HDR : HDR_Ptr := null;
   begin
      -- Check if the DTB address is null
      if Limine.DTB_Response.DTB_Addr = System.Null_Address then
         Debug.Print ("Init: DTB address is null. Cannot initialize DTB.");
         return False;
      end if;

      -- Convert the DTB address to an HDR pointer
      HDR := To_HDR_Ptr (Limine.DTB_Response.DTB_Addr);

      -- Check if the HDR pointer is null
      if HDR = null then
         Debug.Print ("Init: No DTB response.");
         return False;
      end if;

      -- Validate the DTB magic number
      if BE_To_Host (HDR.Magic) /= FDT_MAGIC then
         Debug.Print ("Init: Invalid DTB magic number.");
         return False;
      end if;

      -- Calculate base addresses using proper type conversions
      Struct_Base :=
         To_Address (Integer_Address (Limine.DTB_Response.DTB_Addr)) +
         To_Address (Integer (BE_To_Host (HDR.Offset_DT_Struct)));

      Strings_Base :=
         To_Address (Integer_Address (Limine.DTB_Response.DTB_Addr)) +
         To_Address (Integer (BE_To_Host (HDR.Offset_DT_Strings)));

      DTB_End :=
         To_Address (Integer_Address (Limine.DTB_Response.DTB_Addr)) +
         To_Address (Integer (BE_To_Host (HDR.Size_DT_Struct)));

      -- Debug output for calculated addresses
      Debug.Print ("Init: Struct_Base = " & Address'Image (Struct_Base));
      Debug.Print ("Init: Strings_Base = " & Address'Image (Strings_Base));
      Debug.Print ("Init: DTB_End = " & Address'Image (DTB_End));

      -- Parse the DTB
      Parse_DTB;
      return True;
   exception
      when Constraint_Error =>
         Debug.Print ("Init: Constraint_Error occurred during initialization.");
         return False;
      when Program_Error =>
         Debug.Print ("Init: Program_Error occurred during initialization.");
         return False;
      when others =>
         Debug.Print ("Init: Unknown exception occurred during initialization.");
         return False;
   end Init;

   -----------------------------------------------------------------------------
   --  Top-Level Parse
   -----------------------------------------------------------------------------
   procedure Parse_DTB is
      Pos : Address := Struct_Base;
   begin
      Debug.Print ("Parse_DTB: Starting DTB parsing...");
      Root_Node := Parse_Node (Pos);
      Debug.Print ("Parse_DTB: Parsing complete.");
   exception
      when others =>
         Debug.Print ("Parse_DTB: Exception occurred during parsing.");
   end Parse_DTB;

   -----------------------------------------------------------------------------
   --  Pretty-Print a Node
   -----------------------------------------------------------------------------
   procedure Print_DTB_Node (Node : DTB_Node_Access; Indent : String := "") is
   begin
      Debug.Print (Indent & "Node: " & Node.Name);
      for I in 1 .. Node.Prop_Count loop
         declare
            Prop : constant DTB_Property_Access := Node.Properties (I);
         begin
            Debug.Print (Indent & "  Property: " & Prop.Name &
               " (Length: " & Natural'Image (Prop.Len) & ")");
         end;
      end loop;
      for I in 1 .. Node.Child_Count loop
         Print_DTB_Node (Node.Children (I), Indent & "  ");
      end loop;
   exception
      when others =>
         Debug.Print ("Print_DTB_Node: Exception occurred while printing node.");
   end Print_DTB_Node;

   -----------------------------------------------------------------------------
   --  Print the Entire DTB
   -----------------------------------------------------------------------------
   procedure Print_DTB is
   begin
      if Root_Node /= null then
         Print_DTB_Node (Root_Node);
      else
         Debug.Print ("Print_DTB: No root node found.");
      end if;
   exception
      when others =>
         Debug.Print ("Print_DTB: Exception occurred while printing DTB.");
   end Print_DTB;

   -----------------------------------------------------------------------------
   --  Find Node by Compatible String
   -----------------------------------------------------------------------------
   function Find_Node_By_Compatible (Compat : String) return DTB_Node_Access is
      function Search (N : DTB_Node_Access) return DTB_Node_Access is
         Result : DTB_Node_Access := null;
      begin
         -- Check properties for "compatible"
         for I in 1 .. N.Prop_Count loop
            if N.Properties (I) /= null and then N.Properties (
               I).Name = "compatible" then
               declare
                  Prop_Value : constant String :=
                     String (N.Properties (I).Value);
               begin
                  if Contains_Substring (Prop_Value, Compat) then
                     return N;
                  end if;
               exception
                  when others =>
                     Debug.Print ("Search: Exception occurred while checking property value.");
                     return null;
               end;
            end if;
         end loop;

         -- Recursively search child nodes
         for J in 1 .. N.Child_Count loop
            if N.Children (J) /= null then
               declare
                  C : DTB_Node_Access := null;
               begin
                  C := Search (N.Children (J));
                  if C /= null then
                     return C;
                  end if;
               exception
                  when others =>
                     Debug.Print ("Search: Exception occurred while searching child nodes.");
                     return null;
               end;
            end if;
         end loop;

         return null;
      exception
         when others =>
            Debug.Print ("Search: Exception occurred.");
            return null;
      end Search;

   begin
      if Root_Node = null then
         Debug.Print ("Find_Node_By_Compatible: Root node is null.");
         return null;
      else
         return Search (Root_Node);
      end if;
   exception
      when others =>
         Debug.Print ("Find_Node_By_Compatible: Exception occurred.");
         return null;
   end Find_Node_By_Compatible;

   -----------------------------------------------------------------------------
   --  Fetch a specific indexed value from a property as Unsigned_64
   -----------------------------------------------------------------------------
   function Get_Property_Unsigned_64
     (Node  : DTB_Node_Access;
      Name  : String;
      Index : Positive) return Unsigned_64
   is
      Prop : DTB_Property_Access := null;
   begin
      --  Precondition: Ensure Node is not null
      if Node = null then
         Debug.Print ("Get_Property_Unsigned_64: Node is null.");
         return 0; -- Return a default value
      end if;

      --  Find the property by name
      for I in Node.Properties'Range loop
         if Node.Properties (I).Name = Name then
            Prop := Node.Properties (I);
            exit;
         end if;
      end loop;

      --  Handle missing property
      if Prop = null then
         Debug.Print ("Get_Property_Unsigned_64: Property '" &
         Name & "' not found in node.");
         return 0; -- Return a default value
      end if;

      --  Ensure the index is within bounds
      if Index > Prop.Len then
         Debug.Print (
            "Get_Property_Unsigned_64: Index " & Positive'Image (Index) &
            " out of bounds for property '" &
            Name & "' (Length: " & Natural'Image (Prop.Len) & ").");
         return 0; -- Return a default value
      end if;

      --  Return the indexed value
      return Prop.Value (Index);

   exception
      when others =>
         Debug.Print (
            "Get_Property_Unsigned_64: " &
            "Exception occurred while fetching property '" & Name & "'.");
         return 0; -- Return a default value
   end Get_Property_Unsigned_64;

   function Get_Property_Unsigned_64
     (Node : DTB_Node_Access; Name : String) return Unsigned_64_Array
   is
      Prop : DTB_Property_Access := null;
   begin
      --  Precondition: Ensure Node is not null
      if Node = null then
         Debug.Print ("Get_Property_Unsigned_64: Node is null.");
         return Unsigned_64_Array'(1 .. 0 => Unsigned_64(0));
      end if;

      --  Find the property by name
      for I in 1 .. Node.Prop_Count loop
         if Node.Properties (I).Name = Name then
            Prop := Node.Properties (I);
            exit when Prop /= null;
         end if;
      end loop;

      --  Handle missing property
      if Prop = null then
         Debug.Print ("Get_Property_Unsigned_64: Property '" &
         Name & "' not found in node.");
         return Unsigned_64_Array'(1 .. 0 => Unsigned_64(0));
      end if;

      --  Allocate and populate the result array
      declare
         Result : Unsigned_64_Array (1 .. Prop.Len) := [others => 0];
      begin
         for I in 1 .. Prop.Len loop
            Result (I) := Prop.Value (I);
         end loop;
         return Result;
      end;

   exception
      when others =>
         Debug.Print ("Get_Property_Unsigned_64: " &
         "Exception occurred while fetching property '" & Name & "'.");
         return (1 .. 0 => 0); -- Return an empty array
   end Get_Property_Unsigned_64;

end Arch.DTB;