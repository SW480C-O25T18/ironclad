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
with Interfaces; use Interfaces;
with Arch.Snippets;
with Lib.Panic;
with Arch.Limine;
with Ada.Unchecked_Conversion;

package body Arch.MMU is

   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   function Extract_Physical_Addr (PTE : Page_Table_Entry) return Physical_Address is
      use type Unsigned_64;
      PPN : Unsigned_64;
   begin
      PPN := Unsigned_64(PTE.PPN0) or Shift_Left(Unsigned_64(PTE.PPN1), 9) or Shift_Left(Unsigned_64(PTE.PPN2), 18);
      
      return Physical_Address (Shift_Left (PPN, 12));
   end Extract_Physical_Addr;

   function Extract_Satp_Data (Addr : Unsigned_64) return Satp_Register is
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
      Val :=   (Shift_Left (Unsigned_64(S.Sv_Type), 60))
            or (Shift_Left (Unsigned_64(S.ASID), 44))
            or (Unsigned_64(S.PPN));
      return Val;
   end Combine_Satp_Data;


 ------------------------------------------------------------------
--  RISC-V Sv39  kernel-space page-table initialiser
--  (assumes 4 KiB pages, 3-level page table, direct-map window
--   at Memory_Offset, and a helper Inner_Map_Range that fills
--   leaf PTEs using the Page_Table_Entry record).
------------------------------------------------------------------
function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is

   --  1. Pre-made permission sets, expressed in our Ada record type
   RW_Flags : constant Page_Permissions :=
      (Is_User_Accesible => False,
       Can_Read          => True,
       Can_Write         => True,
       Can_Execute       => False,
       Is_Global         => True);

   RX_Flags : constant Page_Permissions :=
      (Is_User_Accesible => False,
       Can_Read          => True,
       Can_Write         => False,
       Can_Execute       => True,
       Is_Global         => True);

   R_Flags  : constant Page_Permissions :=
      (Is_User_Accesible => False,
       Can_Read          => True,
       Can_Write         => False,
       Can_Execute       => False,
       Is_Global         => True);

   ----------------------------------------------------------------
   --  2. Linker symbols that delimit kernel segments
   ----------------------------------------------------------------
   text_start, text_end,
   rodata_start, rodata_end,
   data_start,  data_end : Character
     with Import, Convention => C;

   TSAddr : constant Integer_Address := To_Integer (text_start'Address);
   OSAddr : constant Integer_Address := To_Integer (rodata_start'Address);
   DSAddr : constant Integer_Address := To_Integer (data_start'Address);

   --  Physical load address as supplied by the boot loader / SBI
   Phys   : constant Integer_Address :=
         To_Integer (Limine.Get_Physical_Address);
begin
   ----------------------------------------------------------------
   --  3. Allocate an empty three-level page table
   ----------------------------------------------------------------
   MMU.Kernel_Table := new Page_Table'
     (Root               => 0,
      Page_Level         => 1,
      Page_Table_Entries =>
  new Page_Level'(others => (others => <>)) ,
      Mutex              => Lib.Synchronization.Unlocked_RW_Lock);

   ----------------------------------------------------------------
   --  4. Identity-map every boot-reported usable RAM region
   --     into the “higher-half” window (direct-map).
   ----------------------------------------------------------------
   for E of Memmap loop
      if not Inner_Map_Range
         (Map            => Kernel_Table,
          Physical_Start => To_Address (To_Integer (E.Start)),
          Virtual_Start  => To_Address (To_Integer (E.Start) + Memory_Offset),
          Length         => Storage_Offset (E.Length),
          Permissions    => RW_Flags,          -- R/W, no execute
          Caching        => Write_Back)
      then
         return False;
      end if;
   end loop;

   ----------------------------------------------------------------
   --  5. Map the kernel image itself with fine-grained permissions
   ----------------------------------------------------------------
   if not Inner_Map_Range
        (Map            => Kernel_Table,
         Physical_Start => To_Address (TSAddr - Kernel_Offset + Phys),
         Virtual_Start  => text_start'Address,
         Length         => text_end'Address - text_start'Address,
         Permissions    => RX_Flags,
         Caching        => Write_Back)
      or else
      not Inner_Map_Range
        (Map            => Kernel_Table,
         Physical_Start => To_Address (OSAddr - Kernel_Offset + Phys),
         Virtual_Start  => rodata_start'Address,
         Length         => rodata_end'Address - rodata_start'Address,
         Permissions    => R_Flags,
         Caching        => Write_Back)
      or else
      not Inner_Map_Range
        (Map            => Kernel_Table,
         Physical_Start => To_Address (DSAddr - Kernel_Offset + Phys),
         Virtual_Start  => data_start'Address,
         Length         => data_end'Address - data_start'Address,
         Permissions    => RW_Flags,
         Caching        => Write_Back)
   then
      return False;
   end if;

   ----------------------------------------------------------------
   --  6. Book-keeping: total bytes of the linked kernel image
   ----------------------------------------------------------------
   Global_Kernel_Usage :=
       Memory.Size (text_end'Address   - text_start'Address)
     + Memory.Size (rodata_end'Address - rodata_start'Address)
     + Memory.Size (data_end'Address   - data_start'Address);

   ----------------------------------------------------------------
   --  7. Activate the new table: MODE=8 (Sv39), ASID=0
   ----------------------------------------------------------------
   return Make_Active (Kernel_Table);

exception
   when Constraint_Error =>
      return False;
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
      ------------------------------------------------------------
      --  Declarations first
      ------------------------------------------------------------
      declare
         PTE_Access : constant Page_Table_Entry_Access :=
         Get_Page (Map, Virt, True);
      begin
         ---------------------------------------------------------
         --  Executable statements
         ---------------------------------------------------------
         if PTE_Access = null then
            return False;
         end if;

         declare
            PTE : Page_Table_Entry renames PTE_Access.all;
            PPN : constant Unsigned_64 := Unsigned_64 (Phys) / 4096;
         begin
            PTE.PPN0 := U9 (PPN              and 16#1FF#);
            PTE.PPN1 := U9 (Shift_Right (PPN,  9) and 16#1FF#);
            PTE.PPN2 := U26(Shift_Right (PPN, 18));
            PTE.R := (if Permissions.Can_Read    then 1 else 0);
            PTE.W := (if Permissions.Can_Write   then 1 else 0);
            PTE.X := (if Permissions.Can_Execute then 1 else 0);
            PTE.U := (if Permissions.Is_User_Accesible then 1 else 0);
            PTE.G := (if Permissions.Is_Global         then 1 else 0);
            PTE.A := 1;
            PTE.D := (if Permissions.Can_Write then 1 else 0);
            PTE.V := 1;
         end;
      end;

      Virt := Virt + Page_Size;
      Phys := Phys + Page_Size;
   end loop;


      return True;
   exception
      when Constraint_Error =>
         return False;
   end Inner_Map_Range;

   -----------------------------------------------------------------
   --  Return: an access value designating the level-0 PTE
   --          that corresponds to Virtual.  If Allocate = False
   --          and any intermediate table is absent, return null.
   -----------------------------------------------------------------
   function Get_Page
   (Map      : Page_Table_Acc;
      Virtual  : Virtual_Address;
      Allocate : Boolean) return Page_Table_Entry_Access
   is
      type Page_Table_Entry_Access is access all Page_Table_Entry;

      Addr64 : constant Unsigned_64 := Unsigned_64 (Virtual);

      --  VPN[2:0] indices (9 bits each)
      VPN2 : constant Unsigned_64 :=
               Shift_Right (Addr64 and Shift_Left (16#1FF#, 30), 30);
      VPN1 : constant Unsigned_64 :=
               Shift_Right (Addr64 and Shift_Left (16#1FF#, 21), 21);
      VPN0 : constant Unsigned_64 :=
               Shift_Right (Addr64 and Shift_Left (16#1FF#, 12), 12);

      --  L2_Phys : Physical_Address :=
      --              To_Integer (Map.Root'Address) - Memory_Offset;
      L2_Phys : Physical_Address := Physical_Address (Map.Root);
      L1_Phys : Physical_Address := Memory.Null_Address;
      L0_Phys : Physical_Address := Memory.Null_Address;
      PTE_VA  : Virtual_Address;
   begin
      ----------------------------------------------------------------
      --  Walk L2 → L1 → L0, allocating new table pages if requested
      ----------------------------------------------------------------
      L1_Phys := Get_Next_Level (L2_Phys, VPN2, Allocate);
      if L1_Phys = Memory.Null_Address then
         return null;
      end if;

      L0_Phys := Get_Next_Level (L1_Phys, VPN1, Allocate);
      if L0_Phys = Memory.Null_Address then
         return null;
      end if;

      ----------------------------------------------------------------
      --  Convert the level-0 table’s *physical* base back to a
      --  kernel-virtual address (direct-map) and add the slot offset.
      ----------------------------------------------------------------
      PTE_VA := L0_Phys + Memory_Offset + Physical_Address (VPN0) * 8;

      return Addr_To_PTE_Access (To_Address (PTE_VA));

   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception in RISC-V page walk");
   end Get_Page;

   -----------------------------------------------------------------
   --  Walk/allocate one step down the Sv39 tree.
   --  Current_Level  : physical base of the table we are looking at.
   --  Index          : VPN component (0‥511) to pick inside that table.
   --  Create_If_Not_Found
   --      = True  → allocate a new 4‑KiB zeroed page if entry empty.
   --      = False → just return Null_Address if entry empty.
   --  Return value   : physical address of the child table, or Null_Address.
   -----------------------------------------------------------------
   function Get_Next_Level
   (Current_Level       : Physical_Address;
      Index               : Unsigned_64;
      Create_If_Not_Found : Boolean) return Physical_Address
   is
      ----------------------------------------------------------------
      --  Location of the 8‑byte slot in the current table
      ----------------------------------------------------------------
      Slot_VA  : constant Virtual_Address :=
      Current_Level + Memory_Offset + Physical_Address (Index * 8);

      --  pragma Import (Ada, Slot_VA);

      --  A typed view of that slot
      Slot : Page_Table_Entry
      with Import, Address => To_Address (Slot_VA);
   begin
      --
      --  Case 1 : already valid and *non‑leaf*
      --           (R=W=X=0 in Sv39 ==> pointer to next level)
      --
      if Slot.V = 1
      and then Slot.R = 0
      and then Slot.W = 0
      and then Slot.X = 0
      then
         return Extract_Physical_Addr (Slot);  -- helper described earlier
      end if;

      --
      --  Case 2 : empty entry and caller wants us to allocate
      --
      if Create_If_Not_Found then
         declare
            Child_Table : constant Page_Level_Acc :=
            new Page_Level'(others => (others => <>)) ;
  

            Child_Phys  : constant Physical_Address :=
            To_Integer (Child_Table.all'Address) - Memory_Offset;

            PPN : constant Unsigned_64 := Unsigned_64 (Child_Phys) / 4096;
         begin
            --  Fill the slot as a non‑leaf PTE (only V‑bit set)
            Slot.PPN0 := U9 (PPN              and 16#1FF#);
            Slot.PPN1 := U9 (Shift_Right (PPN,  9) and 16#1FF#);
            Slot.PPN2 := U26(Shift_Right (PPN, 18));
            Slot.V    := 1;   -- valid
            Slot.R    := 0;   -- non‑leaf
            Slot.W    := 0;
            Slot.X    := 0;
            Slot.U    := 0;
            Slot.G    := 0;
            Slot.A    := 0;
            Slot.D    := 0;

            Global_Table_Usage :=
            Global_Table_Usage + (Page_Level'Size / 8);

            return Child_Phys;
         end;
      end if;

      --
      --  Case 3 : entry missing and caller told us not to create
      --
      return Memory.Null_Address;

   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception when getting next page level");
   end Get_Next_Level;



   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      pragma Unreferenced (Map);
   begin
      Forked := null;
   end Fork_Table;

   procedure Destroy_Table (Map : in out Page_Table_Acc) is
      begin
         Map := null;
--        procedure Free_Page_Table is
--        new Ada.Unchecked_Deallocation(Page_Table, Page_Table_Acc);

--        type Mapping_Range_Acc is access all Mapping_Range;
--        procedure Free_Mapping_Range is
--        new Ada.Unchecked_Deallocation(Mapping_Range, Mapping_Range_Acc);

--        Curr_Range : Mapping_Range_Acc;
--        Last_Range : Mapping_Range_Acc;



--        function Is_Valid_Entry (Map : in out Page_Table_Acc) return Boolean is
--        begin
--        return (Map.Page_Table_Entries.V) /= 0;
--        end Is_Valid_Entry;


--     begin
--   -- todo: adjust to use satp that points to the correct ppn of the root table, should be done in init??
--     -- add lock here
--        Lib.Synchronization.Seize_Writer (Map.Mutex);

--        if Map.Root = null then
--           -- Nothing to do if there's no root table
--           Free_Page_Table (Map);
--           return;
--        end if;
      
--        for i in 1 .. 256 loop -- avoid kernel-space entries Page_Table_Entries
--           declare
--              L3_Entry_Addr : Unsigned_64 := i * 64; -- offset for each page entry is 64 bits up to 512 * 64 = 4096 bits
--              L3_Entry : Page_Table_Entry with Address => L3_Entry_Addr'Address;
--           begin
--              if Is_Valid_Entry(L3_Entry) then
               
--                 L2_Phys := Extract_Physical_Addr(L3_Entry);

--                 --  L2_Virt := To_Address(Memory_Offset + L2_Phys);
--                 declare
--                    L2 : Page_Level;
--                    --  with Import, Address => L2_Virt;

--                 begin
--                    -- Walk all 512 entries in L2
--                    for j in Index_Range loop
                     
--                       L2_Entry_Addr : Unsigned_64 := j * 64;
--                       L2_Entry : Page_Table_Entry with Address => L2_Entry_Addr'Address;

--                       if Is_Valid_Entry(L2_Entry) then
--                          L1_Phys := Extract_Physical_Addr(L2_Entry);
--                          --  L1_Virt := To_Address(Memory_Offset + L1_Phys);

--                          declare
--                             L1 : Page_Level;
--                             --  with Import, Address => L1_Virt;
--                          begin
--                             -- Walk all 512 entries in L1
--                             for k in Index_Range loop
--                                L1_Entry_Addr : Unsigned_64 := k * 64;
--                                L1_Entry : Page_Table_Entry with Address => L1_Entry_Addr'Address;
--                                if Is_Valid_Entry(L1_Entry) then
--                                   -- L1(k) references an actual page frame
--                                   Frame_Phys := Extract_Physical_Addr(L1_Entry) + Memory.Memory_Offset;
--                                   -- Free the final data page
--                                   Memory.Physical.Free(Interfaces.C.size_t(Frame_Phys));
--                                end if;
--                             end loop;

--                             -- Free the entire L1 table page after clearing all entries
--                             Memory.Physical.Free(Interfaces.C.size_t(L1_Phys));
--                          end;
--                       end if;
--                    end loop;

--                    -- Now free the L2 table page
--                    Memory.Physical.Free(Interfaces.C.size_t(L2_Phys));
--                 end;
--              end if;
--           end;
--        end loop;
      
--        Free_Page_Table (Map);

--     exception
--        when Constraint_Error =>
--           --  In case of any pointer issues, just return quietly (or raise an error).
--           return;
   end Destroy_Table;
-----------------------------------------------------------------
--  Install Map as the current address space (Sv39, ASID unchanged).
--  Returns True on success, False if any address arithmetic traps.
-----------------------------------------------------------------
function Make_Active (Map : Page_Table_Acc) return Boolean is
   Physical_Addr : Unsigned_64;
   Satp_Content  : Satp_Register;
   New_PPN       : U44;
   New_SATP      : Unsigned_64;
begin
   ----------------------------------------------------------------
   --  1. Kernel VA → physical → PPN
   ----------------------------------------------------------------
   --  Physical_Addr :=
   --    Unsigned_64 (To_Integer (Map.Page_Table_Entries'Address)
   --                 - Memory.Memory_Offset);
   Physical_Addr := Unsigned_64 (Map.Root);

   New_PPN := U44 (Physical_Addr / 4096);  --  >> 12

   ----------------------------------------------------------------
   --  2. Read current SATP (keeps MODE and ASID intact)
   ----------------------------------------------------------------
   Satp_Content := Extract_Satp_Data (Arch.Snippets.Read_SATP);


   ----------------------------------------------------------------
   --  3. If PPN already matches, nothing to do
   ----------------------------------------------------------------
   if Satp_Content.PPN = New_PPN then
      return True;
   end if;

   ----------------------------------------------------------------
   --  4. Update only the PPN and write back
   ----------------------------------------------------------------
   Satp_Content.PPN := New_PPN;
   New_SATP := Combine_Satp_Data (Satp_Content);

   Arch.Snippets.Write_SATP (New_SATP);
   --  Arch.Snippets.SFence_VMA_All;   -- flush stale TLB entries

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
