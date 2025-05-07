--  arch-mmu.adb: Architecture-specific MMU code.
--  Copyright (C) 2025 streaksu
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

with Interfaces.C;
with Ada.Unchecked_Deallocation;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.APIC;
with Arch.Interrupts;
with Memory.Physical;
with Arch.Limine;
with Lib.Panic;

package body Arch.MMU is
   --  Bits in the 4K page entries.
   Page_P     : constant Unsigned_64 := Shift_Left (1,  0);
   Page_RW    : constant Unsigned_64 := Shift_Left (1,  1);
   Page_U     : constant Unsigned_64 := Shift_Left (1,  2);
   Page_PWT   : constant Unsigned_64 := Shift_Left (1,  3);
   Page_PCD   : constant Unsigned_64 := Shift_Left (1,  4);
   Page_PAT   : constant Unsigned_64 := Shift_Left (1,  7);
   Page_G     : constant Unsigned_64 := Shift_Left (1,  8);
   Page_NX    : constant Unsigned_64 := Shift_Left (1, 63);

   --  Global statistics.
   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is

      --  We define some convenient sets of page-permission flags:
      --   - NX_Flags: Can read/write but cannot execute.
      --   - RX_Flags: Can read/execute but not write.
      --   - R_Flags:  Can read only.

      NX_Flags : constant Page_Permissions :=
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

      R_Flags : constant Page_Permissions :=
         (Is_User_Accesible => False,
         Can_Read          => True,
         Can_Write         => False,
         Can_Execute       => False,
         Is_Global         => True);

      --  The kernel linker typically provides symbols that mark:
      --   * text_start/text_end:   Start/end of the executable code section
      --   * rodata_start/rodata_end: Start/end of the read-only data section
      --   * data_start/data_end:   Start/end of the read/write data section
      --
      --  These addresses come from the linker script or the build system so that
      --  the kernel knows where code vs. read-only data vs. read/write data reside.

      text_start   : Character with Import, Convention => C;
      text_end     : Character with Import, Convention => C;
      rodata_start : Character with Import, Convention => C;
      rodata_end   : Character with Import, Convention => C;
      data_start   : Character with Import, Convention => C;
      data_end     : Character with Import, Convention => C;

      --  Convert those linker-symbol addresses to integer form.
      TSAddr : constant Integer_Address := To_Integer (text_start'Address);
      OSAddr : constant Integer_Address := To_Integer (rodata_start'Address);
      DSAddr : constant Integer_Address := To_Integer (data_start'Address);

      --  Phys is some base physical address. On x86_64-limine, this might
      --  be the load address or an address from the limine boot protocol.
      Phys   : constant Integer_Address :=
         To_Integer (Limine.Get_Physical_Address);

   begin
      --  1) Allocate or create the kernel’s top-level page table data structure
      --     (the “PML4” on x86_64). This will store all mappings needed by the kernel.
      MMU.Kernel_Table := new Page_Table'
         (PML4_Level      => [others => 0],
         Mutex           => Lib.Synchronization.Unlocked_RW_Lock,
         Map_Ranges_Root => null);

      --  2) Map all memory regions from the provided boot memory map.
      --     Each memory region is described by (Start, Length, MemType).
      for E of Memmap loop
         if not Inner_Map_Range
            (Map            => Kernel_Table,
            Physical_Start => To_Address (To_Integer (E.Start)),
            Virtual_Start  => To_Address (To_Integer (E.Start) + Memory_Offset),
            Length         => Storage_Offset (E.Length),
            Permissions    => NX_Flags,    -- read/write but no execute
            Caching        => Write_Back)
         then
            return False;
         end if;
      end loop;

      --  3) Map the kernel’s text, rodata, and data sections into the new page table,
      --     each with appropriate permissions. Typically, we:
      --
      --      * TEXT  => RX_Flags: read + execute, no write
      --      * RODATA => R_Flags: read only
      --      * DATA  => NX_Flags: read + write, no execute
      --
      --  The addresses (TSAddr, OSAddr, DSAddr) have to be offset by the difference
      --  between the load base (Kernel_Offset) and the actual physical location (Phys).
      --  This ensures that the virtual addresses (text_start'Address, etc.) point
      --  to the correct underlying physical addresses.

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
         Permissions    => NX_Flags,
         Caching        => Write_Back)
      then
         return False;
      end if;

      --  4) Track how large these mapped segments are, for memory usage stats.
      Global_Kernel_Usage :=
         Memory.Size (text_end'Address - text_start'Address)
      + Memory.Size (rodata_end'Address - rodata_start'Address)
      + Memory.Size (data_end'Address - data_start'Address);

      --  5) Actually load the new kernel page table into the hardware. On x86_64,
      --     that usually involves writing to CR3 (done by Make_Active).
      return Make_Active (Kernel_Table);

   exception
      when Constraint_Error =>
         return False;
   end Init;


   procedure Fork_Table (Map : Page_Table_Acc; Forked : out Page_Table_Acc) is
      type Page_Data is array (Storage_Count range <>) of Unsigned_8;

      Addr       : System.Address;
      Curr_Range : Mapping_Range_Acc;
      Success    : Boolean;
   begin
      Forked := new Page_Table'
         (PML4_Level      => [others => 0],
          Mutex           => Lib.Synchronization.Unlocked_RW_Lock,
          Map_Ranges_Root => null);

      Lib.Synchronization.Seize_Reader (Map.Mutex);

      --  Clone the higher half, which is the same in all maps.
      Forked.PML4_Level (257 .. 512) := Map.PML4_Level (257 .. 512);

      --  Duplicate the rest of maps, which are mostly going to be lower half.
      Curr_Range := Map.Map_Ranges_Root;
      while Curr_Range /= null loop
         if Curr_Range.Is_Allocated then
            Map_Allocated_Range
               (Map            => Forked,
                Physical_Start => Addr,
                Virtual_Start  => Curr_Range.Virtual_Start,
                Length         => Curr_Range.Length,
                Permissions    => Curr_Range.Flags,
                Success        => Success);
            if not Success then
               Destroy_Table (Forked);
               goto Cleanup;
            end if;

            declare
               New_Data : Page_Data (1 .. Curr_Range.Length)
                  with Import, Address => Addr;
               Original_Data : Page_Data (1 .. Curr_Range.Length)
                  with Import, Address => To_Address
                     (To_Integer (Curr_Range.Physical_Start) + Memory_Offset);
            begin
               New_Data := Original_Data;
            end;
         else
            Map_Range
               (Map            => Forked,
                Physical_Start => Curr_Range.Physical_Start,
                Virtual_Start  => Curr_Range.Virtual_Start,
                Length         => Curr_Range.Length,
                Permissions    => Curr_Range.Flags,
                Success        => Success);
            if not Success then
               Destroy_Table (Forked);
               goto Cleanup;
            end if;
         end if;

         Curr_Range := Curr_Range.Next;
      end loop;

   <<Cleanup>>
      Lib.Synchronization.Release_Reader (Map.Mutex);
   exception
      when Constraint_Error =>
         Forked := null;
   end Fork_Table;

 procedure Destroy_Table (Map : in out Page_Table_Acc) is
   --  These instantiations let us call 'F(...)' to free the Page_Table and
   --  Mapping_Range records themselves from the heap (Ada's Unchecked_Deallocation).
   procedure Free_Page_Table is new Ada.Unchecked_Deallocation(Page_Table, Page_Table_Acc);
   procedure Free_Mapping_Range is new Ada.Unchecked_Deallocation(Mapping_Range, Mapping_Range_Acc);

   Last_Range : Mapping_Range_Acc;
   Curr_Range : Mapping_Range_Acc;
   Discard    : Memory.Size;
begin
   --  1) Acquire exclusive lock (writer lock) so no other code modifies
   --     this page-table structure while we destroy it.
   Lib.Synchronization.Seize_Writer (Map.Mutex);

   --  2) Walk the singly linked list of "mapping ranges," freeing any
   --     user-allocated physical memory and the list nodes.
   Curr_Range := Map.Map_Ranges_Root;
   while Curr_Range /= null loop
      if Curr_Range.Is_Allocated then
         --  Free the physical memory that was allocated for this mapping range.
         Physical.User_Free(To_Integer(Curr_Range.Physical_Start));
      end if;
      Last_Range := Curr_Range;
      Curr_Range := Curr_Range.Next;

      --  Free the mapping-range record from the heap.
      Free_Mapping_Range (Last_Range);
   end loop;

   --  3) Now free all lower-level page tables and page frames in the user-space
   --     portion of the PML4. Typically we iterate over [1..256] to ignore
   --     kernel-space entries in the top half of the PML4. 
   for L3 of Map.PML4_Level(1 .. 256) loop
      --  Clean_Entry(L3) is presumably a helper that masks out flags and returns
      --  just the physical address of the next-level table (if present).
      declare
         A3   : constant Integer_Address := Clean_Entry(L3);
         --  We treat that physical address as another 512-entry array (PML4)
         --  because each x86_64 page-table level has the same layout (512 x 64-bit).
         PML3 : PML4 with Import, Address => To_Address(Memory_Offset + A3);
      begin
         --  Check if the 'present' bit is set in this PML4 entry
         --  (i.e., does it point to a next-level table?).
         if (L3 and Page_P) /= 0 then
            --  Walk the next level (PDP). For each entry, if present,
            --  we go deeper and free the final-level page frames, then free the PDP.
            for L2 of PML3 loop
               declare
                  A2   : constant Integer_Address := Clean_Entry(L2);
                  PML2 : PML4 with Import, Address => To_Address(Memory_Offset + A2);
               begin
                  if (L2 and Page_P) /= 0 then
                     --  If this entry is present, iterate over the final-level
                     --  page table entries (L1), each pointing to page frames.
                     for L1 of PML2 loop
                        --  Extract the physical address from the last-level entry
                        --  and free that frame from physical memory.
                        Memory.Physical.Free(Interfaces.C.size_t(Clean_Entry(L1)));
                     end loop;
                  end if;
                  --  Now free the physical page holding this "PML2" array.
                  Memory.Physical.Free(Interfaces.C.size_t(A2));
               end;
            end loop;
         end if;

         --  Adjust usage statistics (subtract one page-table page).
         Global_Table_Usage := Global_Table_Usage - (PML4'Size / 8);

         --  Free the physical page holding the next-level table (the PDP),
         --  since we are done with it.
         Memory.Physical.Free(Interfaces.C.size_t(A3));
      end;
   end loop;

   --  4) Finally, free the top-level Page_Table record itself.
   Free_Page_Table (Map);
exception
   when Constraint_Error =>
      return;  --  In case of any pointer issues, just return quietly.
end Destroy_Table;


   function Make_Active (Map : Page_Table_Acc) return Boolean is
      Val : Unsigned_64;
   begin
      Val := Unsigned_64 (To_Integer (Map.PML4_Level'Address) - Memory_Offset);
      if Arch.Snippets.Read_CR3 /= Val then
         Arch.Snippets.Write_CR3 (Val);
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
      Virt       : Virtual_Address          := To_Integer (Virtual);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Page_Addr  : Virtual_Address;
      First_Iter : Boolean := True;
   begin
      Physical           := System.Null_Address;
      Is_Mapped          := False;
      Is_User_Accessible := False;
      Is_Readable        := False;
      Is_Writeable       := False;
      Is_Executable      := False;

      while Virt < Final loop
         Lib.Synchronization.Seize_Reader (Map.Mutex);
         Page_Addr := Get_Page (Map, Virt, False);
         Lib.Synchronization.Release_Reader (Map.Mutex);
         declare
            Page : Unsigned_64 with Address => To_Address (Page_Addr), Import;
         begin
            if First_Iter then
               if Page_Addr /= 0 then
                  Physical           := To_Address (Clean_Entry (Page));
                  Is_Mapped          := (Page and Page_P) /= 0;
                  Is_User_Accessible := (Page and Page_U) /= 0;
                  Is_Readable        := True;
                  Is_Writeable       := (Page and Page_RW) /= 0;
                  Is_Executable      := (Page and Page_NX) = 0;
               end if;
               First_Iter := False;
            elsif Page_Addr = 0                                      or else
                  (Is_Mapped          and ((Page and Page_P)   = 0)) or else
                  (Is_User_Accessible and ((Page and Page_U)   = 0)) or else
                  (Is_Writeable       and ((Page and Page_RW)  = 0)) or else
                  (Is_Executable      and ((Page and Page_NX) /= 0))
            then
               Physical           := System.Null_Address;
               Is_Mapped          := False;
               Is_User_Accessible := False;
               Is_Readable        := False;
               Is_Writeable       := False;
               Is_Executable      := False;
               exit;
            end if;
         end;
         Virt := Virt + Page_Size;
      end loop;
   exception
      when Constraint_Error =>
         Physical           := System.Null_Address;
         Is_Mapped          := False;
         Is_User_Accessible := False;
         Is_Readable        := False;
         Is_Writeable       := False;
         Is_Executable      := False;
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
      procedure F is new Ada.Unchecked_Deallocation
         (Mapping_Range, Mapping_Range_Acc);

      New_Range  : Mapping_Range_Acc;
      Last_Range : Mapping_Range_Acc;
      Curr_Range : Mapping_Range_Acc;
   begin
      Success   := False;
      New_Range := new Mapping_Range'
         (Next           => null,
          Is_Allocated   => False,
          Virtual_Start  => Virtual_Start,
          Physical_Start => Physical_Start,
          Length         => Length,
          Flags          => Permissions);

      Curr_Range := Map.Map_Ranges_Root;

      Lib.Synchronization.Seize_Writer (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start <= Virtual_Start and
            Curr_Range.Virtual_Start + Length >= Virtual_Start + Length
         then
            F (New_Range);
            goto Ret;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;

      Curr_Range := New_Range;
      if Map.Map_Ranges_Root = null then
         Map.Map_Ranges_Root := Curr_Range;
      else
         Last_Range.Next := Curr_Range;
      end if;

      Success := Inner_Map_Range
         (Map            => Map,
          Physical_Start => Physical_Start,
          Virtual_Start  => Virtual_Start,
          Length         => Length,
          Permissions    => Permissions,
          Caching        => Caching);

   <<Ret>>
      Lib.Synchronization.Release_Writer (Map.Mutex);
   exception
      when Constraint_Error =>
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
      procedure F is new Ada.Unchecked_Deallocation
         (Mapping_Range, Mapping_Range_Acc);

      Addr       : Virtual_Address;
      New_Range  : Mapping_Range_Acc;
      Last_Range : Mapping_Range_Acc;
      Curr_Range : Mapping_Range_Acc;
   begin
      Memory.Physical.User_Alloc
         (Addr    => Addr,
          Size    => Unsigned_64 (Length),
          Success => Success);
      if not Success then
         Physical_Start := System.Null_Address;
         return;
      end if;

      Success   := False;
      New_Range := new Mapping_Range'
         (Next           => null,
          Is_Allocated   => True,
          Virtual_Start  => Virtual_Start,
          Physical_Start => To_Address (Addr - Memory.Memory_Offset),
          Length         => Length,
          Flags          => Permissions);

      Curr_Range := Map.Map_Ranges_Root;

      Lib.Synchronization.Seize_Writer (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start <= Virtual_Start and
            Curr_Range.Virtual_Start + Length >= Virtual_Start + Length
         then
            goto Ret;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;

      Curr_Range := New_Range;
      if Map.Map_Ranges_Root = null then
         Map.Map_Ranges_Root := Curr_Range;
      else
         Last_Range.Next := Curr_Range;
      end if;

      Success := Inner_Map_Range
         (Map            => Map,
          Physical_Start => To_Address (Addr - Memory.Memory_Offset),
          Virtual_Start  => Virtual_Start,
          Length         => Length,
          Permissions    => Permissions,
          Caching        => Caching);

   <<Ret>>
      if Success then
         declare
            Allocated : array (1 .. Length) of Unsigned_8
               with Import, Address => To_Address (Addr);
         begin
            Allocated      := [others => 0];
            Physical_Start := To_Address (Addr);
         end;
      else
         F (New_Range);
         Memory.Physical.Free (Interfaces.C.size_t (Addr));
         Physical_Start := System.Null_Address;
      end if;
      Lib.Synchronization.Release_Writer (Map.Mutex);
   exception
      when Constraint_Error =>
         Physical_Start := System.Null_Address;
         Success        := False;
   end Map_Allocated_Range;

   procedure Remap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Permissions   : Page_Permissions;
       Success       : out Boolean;
       Caching       : Caching_Model := Write_Back)
   is
      Flags : constant Unsigned_64 := Flags_To_Bitmap (Permissions, Caching);
      Virt       : Virtual_Address          := To_Integer (Virtual_Start);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr       : Virtual_Address;
      Curr_Range : Mapping_Range_Acc;
   begin
      Success    := False;
      Curr_Range := Map.Map_Ranges_Root;

      Lib.Synchronization.Seize_Writer (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start = Virtual_Start and
            Curr_Range.Length        = Length
         then
            Curr_Range.Flags := Permissions;
            goto Actually_Remap;
         end if;

         Curr_Range := Curr_Range.Next;
      end loop;
      goto Ret;

   <<Actually_Remap>>
      while Virt < Final loop
         Addr := Get_Page (Map, Virt, False);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Unsigned_64 (Clean_Entry (Entry_Body)) or Flags;
            end if;
         end;

         Virt := Virt + Page_Size;
      end loop;
      Flush_Global_TLBs (Virtual_Start, Length);
      Success := True;

   <<Ret>>
      Lib.Synchronization.Release_Writer (Map.Mutex);
   exception
      when Constraint_Error =>
         Success := False;
   end Remap_Range;

   procedure Unmap_Range
      (Map           : Page_Table_Acc;
       Virtual_Start : System.Address;
       Length        : Storage_Count;
       Success       : out Boolean)
   is
      procedure F is new Ada.Unchecked_Deallocation
         (Mapping_Range, Mapping_Range_Acc);

      Virt       : Virtual_Address          := To_Integer (Virtual_Start);
      Final      : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr       : Virtual_Address;
      Last_Range : Mapping_Range_Acc := null;
      Curr_Range : Mapping_Range_Acc;
   begin
      Success    := False;
      Curr_Range := Map.Map_Ranges_Root;
      Lib.Synchronization.Seize_Writer (Map.Mutex);

      while Curr_Range /= null loop
         if Curr_Range.Virtual_Start = Virtual_Start and
            Curr_Range.Length        = Length
         then
            if Curr_Range.Is_Allocated then
               Physical.Free (Interfaces.C.size_t
                  (To_Integer (Curr_Range.Physical_Start)));
            end if;
            goto Actually_Unmap;
         end if;

         Last_Range := Curr_Range;
         Curr_Range := Curr_Range.Next;
      end loop;
      goto No_Free_Return;

   <<Actually_Unmap>>
      if Last_Range /= null then
         Last_Range.Next := Curr_Range.Next;
      else
         Map.Map_Ranges_Root := null;
      end if;

      while Virt < Final loop
         Addr := Get_Page (Map, Virt, False);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            if Addr /= 0 then
               Entry_Body := Entry_Body and not Page_P;
            end if;
         end;
         Virt := Virt + Page_Size;
      end loop;
      Flush_Global_TLBs (Virtual_Start, Length);
      Success := True;

      Lib.Synchronization.Release_Writer (Map.Mutex);
      F (Curr_Range);
      return;

   <<No_Free_Return>>
      Lib.Synchronization.Release_Writer (Map.Mutex);

   exception
      when Constraint_Error =>
         Success := False;
   end Unmap_Range;

   procedure Get_User_Mapped_Size (Map : Page_Table_Acc; Sz : out Unsigned_64)
   is
      Curr_Range : Mapping_Range_Acc;
   begin
      Lib.Synchronization.Seize_Reader (Map.Mutex);
      Sz := 0;
      Curr_Range := Map.Map_Ranges_Root;
      while Curr_Range /= null loop
         Sz         := Sz + Unsigned_64 (Curr_Range.Length);
         Curr_Range := Curr_Range.Next;
      end loop;
      Lib.Synchronization.Release_Reader (Map.Mutex);
   exception
      when Constraint_Error =>
         Sz := 0;
   end Get_User_Mapped_Size;

   procedure Get_Statistics (Stats : out Virtual_Statistics) is
      Val1, Val2 : Memory.Size;
   begin
      Val1 := Global_Kernel_Usage;
      Val2 := Global_Table_Usage;
      Stats := (Val1, Val2, 0);
   end Get_Statistics;
   ----------------------------------------------------------------------------
   function Clean_Entry (Entry_Body : Unsigned_64) return Physical_Address is
   begin
      return Physical_Address (Entry_Body and 16#FFFFFFF000#);
   exception
      when Constraint_Error =>
         return 0;
   end Clean_Entry;

   function Get_Next_Level
      (Current_Level       : Physical_Address;
       Index               : Unsigned_64;
       Create_If_Not_Found : Boolean) return Physical_Address
   is
      Discard : Memory.Size;
   begin
      declare
         Entry_Addr : constant Virtual_Address :=
            Current_Level + Memory_Offset + Physical_Address (Index * 8);
         Entry_Body : Unsigned_64
            with Address => To_Address (Entry_Addr), Import;
      begin
         --  Check whether the entry is present.
         if (Entry_Body and Page_P) /= 0 then
            return Clean_Entry (Entry_Body);
         elsif Create_If_Not_Found then
            --  Allocate and put some default flags.
            declare
               New_Entry      : constant PML4_Acc := new PML4'(others => 0);
               New_Entry_Addr : constant Physical_Address :=
                  To_Integer (New_Entry.all'Address) - Memory_Offset;
            begin
               Global_Table_Usage := Global_Table_Usage + (PML4'Size / 8);
               Entry_Body := Unsigned_64 (New_Entry_Addr) or Page_P or
                             Page_U or Page_RW;
               return New_Entry_Addr;
            end;
         end if;
      end;
      return Memory.Null_Address;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception when getting next page level");
   end Get_Next_Level;

   function Get_Page
      (Map      : Page_Table_Acc;
       Virtual  : Virtual_Address;
       Allocate : Boolean) return Virtual_Address
   is
      Addr : constant Unsigned_64 := Unsigned_64 (Virtual);
      PML4_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 39), 39);
      PML3_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 30), 30);
      PML2_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 21), 21);
      PML1_Entry : constant Unsigned_64 :=
         Shift_Right (Addr and Shift_Left (16#1FF#, 12), 12);
      Addr4, Addr3, Addr2, Addr1 : Physical_Address := Memory.Null_Address;
   begin
      --  Find the entries.
      Addr4 := To_Integer (Map.PML4_Level'Address) - Memory_Offset;
      Addr3 := Get_Next_Level (Addr4, PML4_Entry, Allocate);
      if Addr3 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Addr2 := Get_Next_Level (Addr3, PML3_Entry, Allocate);
      if Addr2 = Memory.Null_Address then
         goto Error_Return;
      end if;
      Addr1 := Get_Next_Level (Addr2, PML2_Entry, Allocate);
      if Addr1 = Memory.Null_Address then
         goto Error_Return;
      end if;
      return Addr1 + Memory_Offset + (Physical_Address (PML1_Entry) * 8);

   <<Error_Return>>
      return Memory.Null_Address;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception when fetching/allocating page");
   end Get_Page;

   function Inner_Map_Range
      (Map            : Page_Table_Acc;
       Physical_Start : System.Address;
       Virtual_Start  : System.Address;
       Length         : Storage_Count;
       Permissions    : Page_Permissions;
       Caching        : Caching_Model) return Boolean
   is
      Flags : constant Unsigned_64 := Flags_To_Bitmap (Permissions, Caching);
      Virt  : Virtual_Address          := To_Integer (Virtual_Start);
      Phys  : Virtual_Address          := To_Integer (Physical_Start);
      Final : constant Virtual_Address := Virt + Virtual_Address (Length);
      Addr  : Virtual_Address;
   begin
      while Virt < Final loop
         Addr := Get_Page (Map, Virt, True);

         declare
            Entry_Body : Unsigned_64 with Address => To_Address (Addr), Import;
         begin
            Entry_Body := Unsigned_64 (Phys) or Flags;
         end;

         Virt := Virt + Page_Size;
         Phys := Phys + Page_Size;
      end loop;
      return True;
   end Inner_Map_Range;

   function Flags_To_Bitmap
      (Perm    : Page_Permissions;
       Caching : Caching_Model) return Unsigned_64
   is
      Result : Unsigned_64 := 0;
   begin
      Result :=
         (if Perm.Can_Execute       then 0                  else Page_NX) or
         (if Perm.Can_Write         then Page_RW            else       0) or
         (if Perm.Is_Global         then Page_G             else       0) or
         (if Perm.Is_User_Accesible then Page_U             else       0) or
         Page_P;

      case Caching is
         when Write_Back      => null;
         when Write_Through   => Result := Result or Page_PWT;
         when Write_Combining => Result := Result or Page_PAT or Page_PWT;
         when Uncacheable     => Result := Result or Page_PWT or Page_PCD;
      end case;

      return Result;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Exception when translating bitmap flags");
   end Flags_To_Bitmap;

   procedure Flush_Global_TLBs (Addr : System.Address; Len : Storage_Count) is
      Final : constant System.Address := Addr + Len;
      Curr  :          System.Address := Addr;
   begin
      --  First, invalidate for ourselves.
      while To_Integer (Curr) < To_Integer (Final) loop
         Snippets.Invalidate_Page (To_Integer (Curr));
         Curr := Curr + Page_Size;
      end loop;

      --  If we are running on a process, and said process is running with more
      --  than one thread, we need to invalidate using funky IPIs.
      if CPU.Core_Locals /= null then
         for I in CPU.Core_Locals.all'Range loop
            if I /= CPU.Get_Local.Number then
               CPU.Core_Locals (I).Invalidate_Start := Addr;
               CPU.Core_Locals (I).Invalidate_End   := Final;
               APIC.LAPIC_Send_IPI
                  (CPU.Core_Locals (I).LAPIC_ID,
                   Interrupts.Invalidate_Interrupt);
            end if;
         end loop;
      end if;
   exception
      when Constraint_Error =>
         Lib.Panic.Hard_Panic ("Could not flish global TLBs");
   end Flush_Global_TLBs;
end Arch.MMU;
