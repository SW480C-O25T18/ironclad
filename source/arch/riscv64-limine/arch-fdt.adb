--  arch-fdt.adb: Flattened Device Tree parser for riscv64-limine
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
with Arch.Debug;
with Lib.Panic;

package body Arch.FDT is
   pragma Restrictions (No_Exception_Propagation);

   --  FDT token values
   FDT_BEGIN_NODE : constant Interfaces.Unsigned_32 := 1;
   FDT_END_NODE   : constant Interfaces.Unsigned_32 := 2;
   FDT_PROP       : constant Interfaces.Unsigned_32 := 3;
   FDT_NOP        : constant Interfaces.Unsigned_32 := 4;
   FDT_END        : constant Interfaces.Unsigned_32 := 9;

   --  DTB header offsets
   Header_Magic_Off     : constant := 0;
   Header_OffStruct_Off : constant := 8;
   Header_OffStrings_Off: constant := 12;

   --  Magic value
   DTB_Magic : constant Interfaces.Unsigned_32 := 16#D00DFEED#;

   --  Helper: read a byte from DTB
   function Read8 (Addr : System.Address) return Interfaces.Unsigned_8 is
      U : System.Storage_Elements.Storage_Unit;
   begin
      U := System.Storage_Elements.Read (Addr);
      return Interfaces.Unsigned_8 (U);
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Arch.FDT.Read8 at " & Interfaces.Unsigned_64'Image (
            System.Storage_Elements.Address_To_Integer (Addr)));
         Lib.Panic.Hard_Panic ("FDT Read8 error");
   end Read8;

   --  Helper: read big-endian 32-bit
   function Read_BE32 (Addr : System.Address) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.Unsigned_32 (Read8 (Addr)) * 16#1000000# +
             Interfaces.Unsigned_32 (Read8 (Addr + 1)) * 16#10000#   +
             Interfaces.Unsigned_32 (Read8 (Addr + 2)) * 16#100#     +
             Interfaces.Unsigned_32 (Read8 (Addr + 3));
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Arch.FDT.Read_BE32 at " & Interfaces.Unsigned_64'Image (
            System.Storage_Elements.Address_To_Integer (Addr)));
         Lib.Panic.Hard_Panic ("FDT Read_BE32 error");
   end Read_BE32;

   --  Helper: read big-endian 64-bit
   function Read_BE64 (Addr : System.Address) return Interfaces.Unsigned_64 is
   begin
      return Interfaces.Unsigned_64 (Read_BE32 (Addr)) * 16#100000000# +
             Interfaces.Unsigned_64 (Read_BE32 (Addr + 4));
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Arch.FDT.Read_BE64 at " & Interfaces.Unsigned_64'Image (
            System.Storage_Elements.Address_To_Integer (Addr)));
         Lib.Panic.Hard_Panic ("FDT Read_BE64 error");
   end Read_BE64;

   --  Align to 4-byte boundary
   function Align4 (Addr : System.Address) return System.Address is
      Int_Addr : Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (
         System.Storage_Elements.Address_To_Integer (Addr));
      Remainder: Interfaces.Unsigned_64 := Int_Addr mod 4;
   begin
      if Remainder = 0 then
         return Addr;
      else
         return System.Storage_Elements.Integer_To_Address (
            Int_Addr + (4 - Remainder));
      end if;
   end Align4;

   --  Fixed-capacity Reg_Vector
   Max_Reg_Entries : constant Positive := 16;
   type Reg_Entry is record
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64;
   end record with Convention => C;

   type Reg_Vector is record
      Entries : array (Positive range 1 .. Max_Reg_Entries) of Reg_Entry;
      Length  : Natural := 0;
   end record;

   procedure Append_Reg (Vec : in out Reg_Vector; E : in Reg_Entry) is
   begin
      if Vec.Length < Vec.Entries'Length then
         Vec.Length := Vec.Length + 1;
         Vec.Entries (Vec.Length) := E;
      else
         Arch.Debug.Print ("[WARN] Arch.FDT.Append_Reg overflow");
      end if;
   end Append_Reg;

   --  Fixed-capacity Int_Vector
   Max_Int_Cells : constant Positive := 8;
   type Int_Vector is record
      Cells  : array (Positive range 1 .. Max_Int_Cells) of Interfaces.Unsigned_32;
      Length : Natural := 0;
   end record;

   procedure Append_Int_Cell (Vec : in out Int_Vector; C : in Interfaces.Unsigned_32) is
   begin
      if Vec.Length < Vec.Cells'Length then
         Vec.Length := Vec.Length + 1;
         Vec.Cells (Vec.Length) := C;
      else
         Arch.Debug.Print ("[WARN] Arch.FDT.Append_Int_Cell overflow");
      end if;
   end Append_Int_Cell;

   --  Opaque handle
   type Handle is record
      Base_Address : System.Address;
      Off_Struct   : Interfaces.Unsigned_32;
      Off_Strings  : Interfaces.Unsigned_32;
   end record;

   --  Open DTB
   function Open (Base : System.Address) return Handle is
      Magic      : Interfaces.Unsigned_32;
      OffStruct  : Interfaces.Unsigned_32;
      OffStrings : Interfaces.Unsigned_32;
      H          : Handle;
   begin
      Magic := Read_BE32 (Base + Header_Magic_Off);
      if Magic /= DTB_Magic then
         Arch.Debug.Print ("[ERROR] Arch.FDT.Open invalid magic");
         return (Base_Address => System.Null_Address, Off_Struct => 0, Off_Strings => 0);
      end if;
      OffStruct  := Read_BE32 (Base + Header_OffStruct_Off);
      OffStrings := Read_BE32 (Base + Header_OffStrings_Off);
      H := (Base_Address => Base, Off_Struct => OffStruct, Off_Strings => OffStrings);
      Arch.Debug.Print ("[TRACE] Arch.FDT.Open success");
      return H;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Arch.FDT.Open exception");
         Lib.Panic.Hard_Panic ("FDT Open exception");
   end Open;

   --  Close DTB
   procedure Close (H : in out Handle) is
   begin
      Arch.Debug.Print ("[TRACE] Arch.FDT.Close");
   exception
      when others =>
         Arch.Debug.Print ("[WARN] Arch.FDT.Close exception");
   end Close;

   --  Read property name
   function Read_Property_Name (H : Handle; StrOff : Interfaces.Unsigned_32) return String is
      Buf : String (1 .. 64);
      Len : Positive := 0;
   begin
      loop
         exit when Read8 (H.Base_Address + System.Storage_Elements.Integer_To_Address (
               H.Off_Strings + StrOff) + Len) = 0;
         if Len < Buf'Length then
            Len := Len + 1;
            Buf (Len) := Character'Val (Integer (Read8 (H.Base_Address + System.Storage_Elements.Address_To_Integer (
               H.Base_Address) + H.Off_Strings + StrOff + Len)));
         end if;
      end loop;
      return Buf (1 .. Len);
   exception
      when others =>
         Arch.Debug.Print ("[WARN] Arch.FDT.Read_Property_Name exception");
         return "";
   end Read_Property_Name;

   --  Find node by compatible
   function Find_Node (H : Handle; Compatible : String) return System.Address is
      Ptr    : System.Address := H.Base_Address + System.Storage_Elements.Integer_To_Address (H.Off_Struct);
      EndPtr : System.Address := H.Base_Address + System.Storage_Elements.Integer_To_Address (H.Off_Strings);
      Token  : Interfaces.Unsigned_32;
      Name   : String (1 .. 64);
      NameLen: Positive;
      DataLen: Interfaces.Unsigned_32;
      StrOff : Interfaces.Unsigned_32;
      VPtr   : System.Address;
      EndV   : System.Address;
      SBuf   : String (1 .. 64);
      SLen   : Positive;
   begin
      Arch.Debug.Print ("[TRACE] Arch.FDT.Find_Node " & Compatible);
      while Ptr < EndPtr loop
         Token := Read_BE32 (Ptr);
         Ptr := Ptr + 4;
         case Token is
            when FDT_BEGIN_NODE =>
               -- skip node name
               loop exit when Read8 (Ptr) = 0; Ptr := Ptr + 1; end loop;
               Ptr := Align4 (Ptr + 1);
            when FDT_PROP =>
               DataLen := Read_BE32 (Ptr);
               StrOff  := Read_BE32 (Ptr + 4);
               Ptr := Ptr + 8;
               NameLen := 0;
               for I in 0 .. Name'Length-1 loop
                  exit when Read8 (H.Base_Address + System.Storage_Elements.Integer_To_Address (
                     H.Off_Strings + StrOff) + I) = 0;
                  NameLen := NameLen + 1;
                  Name (NameLen) := Character'Val (Integer (Read8 (H.Base_Address + System.Storage_Elements.Integer_To_Address (
                     H.Off_Strings + StrOff) + I)));
               end loop;
               if Name (1 .. NameLen) = "compatible" then
                  VPtr := Ptr;
                  EndV := Ptr + System.Storage_Elements.Integer_To_Address (DataLen);
                  while VPtr < EndV loop
                     SLen := 0;
                     for I in 0 .. SBuf'Length-1 loop
                        exit when Read8 (VPtr + I) = 0;
                        SLen := SLen + 1;
                        SBuf (SLen) := Character'Val (Integer (Read8 (VPtr + I)));
                     end loop;
                     if SBuf (1 .. SLen) = Compatible then
                        Arch.Debug.Print ("[TRACE] Found node " & Compatible);
                        return Ptr;
                     end if;
                     VPtr := VPtr + SLen + 1;
                  end loop;
               end if;
               Ptr := Align4 (Ptr + System.Storage_Elements.Integer_To_Address (DataLen));
            when FDT_END_NODE | FDT_NOP => null;
            when FDT_END => exit;
            when others => null;
         end case;
      end loop;
      return System.Null_Address;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] Find_Node exception");
         return System.Null_Address;
   end Find_Node;

   --  Read a 32-bit property
   function Get_Property_U32
     (H : Handle; Node : System.Address; Name : String; Default : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32 is
      Ptr     : System.Address := H.Base_Address + System.Storage_Elements.Integer_To_Address (H.Off_Struct);
      EndPtr  : System.Address := H.Base_Address + System.Storage_Elements.Integer_To_Address (H.Off_Strings);
      Token   : Interfaces.Unsigned_32;
      DataLen : Interfaces.Unsigned_32;
      StrOff  : Interfaces.Unsigned_32;
      Val     : Interfaces.Unsigned_32 := Default;
   begin
      while Ptr < EndPtr loop
         Token := Read_BE32 (Ptr);
         Ptr := Ptr + 4;
         if Token = FDT_PROP then
            DataLen := Read_BE32 (Ptr);
            StrOff  := Read_BE32 (Ptr + 4);
            Ptr := Ptr + 8;
            if Read_Property_Name (H, StrOff) = Name then
               Val := Read_BE32 (Ptr);
               Arch.Debug.Print ("[TRACE] Prop " & Name & "=" & Interfaces.Unsigned_32'Image (Val));
               return Val;
            end if;
            Ptr := Align4 (Ptr + System.Storage_Elements.Integer_To_Address (DataLen));
         elsif Token = FDT_END then
            exit;
         else
            Ptr := Align4 (Ptr);
         end if;
      end loop;
      Arch.Debug.Print ("[TRACE] Prop default " & Name & "=" & Interfaces.Unsigned_32'Image (Default));
      return Val;
   exception
      when others =>
         Arch.Debug.Print ("[WARN] Get_Property_U32 exception " & Name);
         return Default;
   end Get_Property_U32;

   --  Read 'reg' entries
   function Get_Reg (H : Handle; Node : System.Address) return Reg_Vector is
      Vec     : Reg_Vector;
      DataLen : Interfaces.Unsigned_32 := Read_BE32 (Node - 8);
      Ptr     : System.Address := Node;
      EndPtr  : System.Address := Node + System.Storage_Elements.Integer_To_Address (DataLen);
      E       : Reg_Entry;
   begin
      while Ptr < EndPtr loop
         E.Address := Read_BE64 (Ptr);
         E.Size    := Read_BE64 (Ptr + 8);
         Append_Reg (Vec, E);
         Ptr := Ptr + 16;
      end loop;
      return Vec;
   exception
      when others =>
         Arch.Debug.Print ("[WARN] Get_Reg exception");
         return Vec;
   end Get_Reg;

   --  Read interrupts array
   function Get_Interrupts
     (H : Handle; Node : System.Address; Name : String)
     return Int_Vector is
      Vec     : Int_Vector;
      DataLen : Interfaces.Unsigned_32 := Read_BE32 (Node - 8);
      Ptr     : System.Address := Node;
      EndPtr  : System.Address := Node + System.Storage_Elements.Integer_To_Address (DataLen);
   begin
      while Ptr < EndPtr loop
         Append_Int_Cell (Vec, Read_BE32 (Ptr));
         Ptr := Ptr + 4;
      end loop;
      return Vec;
   exception
      when others =>
         Arch.Debug.Print ("[WARN] Get_Interrupts exception");
         return Vec;
   end Get_Interrupts;

end Arch.FDT;