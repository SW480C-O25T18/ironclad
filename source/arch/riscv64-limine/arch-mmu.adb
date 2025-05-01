--  arch-mmu.adb: Architecture-specific MMU code.
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

with Memory;
package body Arch.MMU is

   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   function Extract_Physical_Addr (PTE : Page_Table_Entry) return Interfaces.C.size_t is
      use type Interfaces.C.size_t;
      use type u64;
      PPN : u64;
   begin
      PPN := u64(PTE.PPN0) or Shift_Left(u64(PTE.PPN1), 9) or Shift_Left(u64(PTE.PPN2), 18);
      
      return Interfaces.C.size_t(Shift_Left(PPN, 12)); -- change to use Physical_Address()
   end Extract_Physical_Addr;

   function Extract_Satp_Data (Satp_Content : u64) return Satp_Register is
      Result : Satp_Register;
   begin
      -- Bits 60..63 => Sv_Type
      Result.Sv_Type := U4((Shift_Right (Addr, 60)) and 16#F#); 

      -- Bits 44..59 => ASID (16 bits)
      Result.ASID := U16((Shift_Right(Addr, 44)) and 16#FFFF#); 

      -- Bits 0..43 => PPN (44 bits)
      -- We can mask off the low 44 bits with (1 << 44) - 1
      Result.PPN := U44(Addr and ((Shift_Left(1,44)) - 1)); 

      return Result;
   end Extract_Satp_Data;

   function Combine_Satp_Data(S : Satp_Register) return Unsigned_64 is
      Val : Unsigned_64;
   begin
      Val :=   (Shift_Left (u64(S.Sv_Type), 60))
            or (Shift_Left (u64(S.ASID), 44))
            or (Unsigned_64(S.PPN));
      return Val;
   end Combine_Satp_Data;


   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
   begin
      return True;
   end Init;

      function Inner_Map_Range
   (Map            : Page_Table_Acc;
      Physical_Start : System.Address;
      Virtual_Start  : System.Address;
      Length         : Storage_Count;
      Permissions    : Page_Permissions;
      Caching        : Caching_Model) return Boolean
   is
      --  Flags : constant Unsigned_64 := Flags_To_Bitmap (Permissions, Caching);

      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Phys  : Virtual_Address          := To_Integer (Physical_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
   begin
      while Virt < Final loop
       Addr := Get_Page (Map, Virt, True);

      declare
         PTE : Page_Table_Entry
         with Address => To_Address (Addr), Import;
         PPN : constant Unsigned_64 := Unsigned_64 (Phys) / 4096; -- phys >> 12
      begin
         --  Fill the 44-bit physical-page number fields
         PTE.PPN0 := U9 (PPN and   16#1FF#);          -- bits  0..8
         PTE.PPN1 := U9 (Shift_Right (PPN,  9) and 16#1FF#); -- bits  9..17
         PTE.PPN2 := U26(Shift_Right (PPN, 18));           -- bits 18..43

         --  Access permission bits
         PTE.R := (if Permissions.Can_Read    then 1 else 0);
         PTE.W := (if Permissions.Can_Write   then 1 else 0);
         PTE.X := (if Permissions.Can_Execute then 1 else 0);
         PTE.U := (if Permissions.Is_User_Accesible then 1 else 0);
         PTE.G := (if Permissions.Is_Global   then 1 else 0);

         --  Software must set A and D the first time it installs a page
         PTE.A := 1;   -- “accessed”
         PTE.D := (if Permissions.Can_Write then 1 else 0); -- writable ⇒ dirty

         PTE.V := 1;   -- valid
      end;


         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Inner_Map_Range;

   function Get_Page
  (Map      : Page_Table_Acc;
   Virtual  : Virtual_Address;
   Allocate : Boolean) return Virtual_Address
is
   Addr64 : constant Unsigned_64 := Unsigned_64 (Virtual);

   --  VPN[2:0] indices (9 bits each)
   VPN2 : constant Unsigned_64 :=
              Shift_Right (Addr64 and Shift_Left (16#1FF#, 30), 30);
   VPN1 : constant Unsigned_64 :=
              Shift_Right (Addr64 and Shift_Left (16#1FF#, 21), 21);
   VPN0 : constant Unsigned_64 :=
              Shift_Right (Addr64 and Shift_Left (16#1FF#, 12), 12);

   L2_Phys : Physical_Address :=
                To_Integer (Map.Root'Address) - Memory_Offset;
   L1_Phys : Physical_Address := Memory.Null_Address;
   L0_Phys : Physical_Address := Memory.Null_Address;
begin
   --  Walk level-2 → level-1 → level-0
   L1_Phys := Get_Next_Level (L2_Phys, VPN2, Allocate);
   if L1_Phys = Memory.Null_Address then goto Error_Return; end if;

   L0_Phys := Get_Next_Level (L1_Phys, VPN1, Allocate);
   if L0_Phys = Memory.Null_Address then goto Error_Return; end if;

   --  Return *address* of the final PTE (still physical)
   return L0_Phys + Memory_Offset + (Physical_Address (VPN0) * 8);

<<Error_Return>>
   return Memory.Null_Address;
exception
   when Constraint_Error =>
      Lib.Panic.Hard_Panic ("Exception in RISC-V page walk");
end Get_Page;



   --  function Flags_To_Bitmap
   --  (Perm    : Page_Permissions;
   --     Caching : Caching_Model) return Unsigned_64
   --  is
   --     Result : Unsigned_64 := Page_V or Page_A or Page_D;  -- always valid & A/D
   --  begin
   --     --  Basic access bits
   --     if Perm.Can_Read    then Result := Result or Page_R; end if;
   --     if Perm.Can_Write   then Result := Result or Page_W; end if;
   --     if Perm.Can_Execute then Result := Result or Page_X; end if;
   --     if Perm.Is_User_Accesible then Result := Result or Page_U; end if;
   --     if Perm.Is_Global        then Result := Result or Page_G; end if;

   --     --  RISC-V has no per-PTE cache-policy bits; ignore 'Caching'.
   --     return Result;
   --  exception
   --     when Constraint_Error =>
   --        Lib.Panic.Hard_Panic ("Exception building RISC-V PTE flags");
   --  end Flags_To_Bitmap;


   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      pragma Unreferenced (Map);
   begin
      Forked := null;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      procedure Free_Page_Table is
      new Ada.Unchecked_Deallocation(Page_Table, Page_Table_Acc);

      type Mapping_Range_Acc is access all Mapping_Range;
      procedure Free_Mapping_Range is
      new Ada.Unchecked_Deallocation(Mapping_Range, Mapping_Range_Acc);

      Curr_Range : Mapping_Range_Acc;
      Last_Range : Mapping_Range_Acc;



      function Is_Valid_Entry (Map : in out Page_Table_Acc return Boolean is
      begin
      return (Map.Page_Table_Entries.V) /= 0;
      end Is_Valid_Entry;


   begin
 -- todo: adjust to use satp that points to the correct ppn of the root table, should be done in init??
   -- add lock here
      Lib.Synchronization.Seize_Writer (Map.Mutex);

      if Map.Root = null then
         -- Nothing to do if there's no root table
         Free_Page_Table (Map);
         return;
      end if;
      
      for i in 1 .. 256 loop -- avoid kernel-space entries Page_Table_Entries
         declare
            L3_Entry_Addr : Unsigned_64 := i * 64; -- offset for each page entry is 64 bits up to 512 * 64 = 4096 bits
            L3_Entry : Page_Table_Entry with Address => L3_Entry_Addr'Address;
         begin
            if Is_Valid_Entry(L3_Entry) then
               
               L2_Phys := Extract_Physical_Addr(L3_Entry);

               --  L2_Virt := To_Address(Memory_Offset + L2_Phys);
               declare
                  L2 : Page_Level;
                  --  with Import, Address => L2_Virt;

               begin
                  -- Walk all 512 entries in L2
                  for j in Index_Range loop
                     
                     L2_Entry_Addr : Unsigned_64 := j * 64;
                     L2_Entry : Page_Table_Entry with Address => L2_Entry_Addr'Address;

                     if Is_Valid_Entry(L2_Entry) then
                        L1_Phys := Extract_Physical_Addr(L2_Entry);
                        --  L1_Virt := To_Address(Memory_Offset + L1_Phys);

                        declare
                           L1 : Page_Level;
                           --  with Import, Address => L1_Virt;
                        begin
                           -- Walk all 512 entries in L1
                           for k in Index_Range loop
                              L1_Entry_Addr : Unsigned_64 := k * 64;
                              L1_Entry : Page_Table_Entry with Address => L1_Entry_Addr'Address;
                              if Is_Valid_Entry(L1_Entry) then
                                 -- L1(k) references an actual page frame
                                 Frame_Phys := Extract_Physical_Addr(L1_Entry) + Memory.Memory_Offset;
                                 -- Free the final data page
                                 Memory.Physical.Free(Interfaces.C.size_t(Frame_Phys));
                              end if;
                           end loop;

                           -- Free the entire L1 table page after clearing all entries
                           Memory.Physical.Free(Interfaces.C.size_t(L1_Phys));
                        end;
                     end if;
                  end loop;

                  -- Now free the L2 table page
                  Memory.Physical.Free(Interfaces.C.size_t(L2_Phys));
               end;
            end if;
         end;
      end loop;
      
      Free_Page_Table (Map);

   exception
      when Constraint_Error =>
         --  In case of any pointer issues, just return quietly (or raise an error).
         return;
   end Destroy_Table;

   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Physical_Addr : Unsigned_64;
      Satp_Content : Satp_Register;
      New_PPN : U44;
      New_SATP : Satp_Register;
   begin
      Physical_Addr := Unsigned_64 (To_Integer (Map.Page_Table_Entries'Address) - Memory.Memory_Offset);
      Satp_Content := Arch.Snippets.Read_SATP;
      
      New_PPN := U44(Physical_Addr / 4096);
      if Satp_Content.PPN /= New_PPN then
         Satp_Content.PPN := New_PPN;
         New_SATP := Combine_Satp_Data (Satp_Content);
         Arch.Snippets.Write_SATP (New_SATP);
      end if;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Make_Active;

   procedure Translate_Address
      (Map                : Page_Table_Acc;
       Virtual            : System.Address;
       Length             : Storage_Count;
       Physical           : out System.Address;
       Is_Mapped          : out Boolean;
       Is_User_Accessible : out Boolean;
       Is_Readable        : out Boolean;
       Is_Writeable       : out Boolean;
       Is_Executable      : out Boolean)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual);
      pragma Unreferenced (Length);
   begin
      Physical  := System.Null_Address;
      Is_Mapped := True;
      Is_User_Accessible := True;
      Is_Readable := True;
      Is_Writeable := True;
      Is_Executable := False;
   end Translate_Address;

   procedure Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Physical_Start);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Success := False;
   end Map_Range;

   procedure Map_Allocated_Range
      (Map            : Page_Table_Acc;
       Physical_Start : out System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Success        : out Boolean;
       Caching        : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Physical_Start := System.Null_Address;
      Success        := True;
   end Map_Allocated_Range;

   procedure Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Success       : out Boolean;
       Caching       : Caching_Model := Write_Back)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
      pragma Unreferenced (Permissions);
      pragma Unreferenced (Caching);
   begin
      Success := False;
   end Remap_Range;

   procedure Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Success       : out Boolean)
   is
      pragma Unreferenced (Map);
      pragma Unreferenced (Virtual_Start);
      pragma Unreferenced (Length);
   begin
      Success := True;
   end Unmap_Range;

   procedure Get_User_Mapped_Size (Map : Page_Table_Acc; Sz : out Unsigned_64)
   is
      pragma Unreferenced (Map);
   begin
      Sz := 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
      Val1, Val2 : Memory.Size;
   begin
      Val1 := Global_Kernel_Usage;
      Val2 := Global_Table_Usage;
      Stats := (Val1, Val2, 0);
   end Get_Statistics;

end Arch.MMU;
