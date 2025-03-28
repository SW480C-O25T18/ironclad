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

package body Arch.MMU is

   Global_Kernel_Usage : Memory.Size := 0;
   Global_Table_Usage  : Memory.Size := 0;

   function Init (Memmap : Arch.Boot_Memory_Map) return Boolean is
      pragma Unreferenced (Memmap);
   begin
      return True;
   end Init;

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


      function Is_Valid_Entry (P : Unsigned_64) return Boolean is

      begin
      return (P and 16#001#) /= 0;
      end Is_Valid_Entry;

      function Extract_Physical_Addr (P : Unsigned_64) return Interfaces.C.size_t is
      PPN : Unsigned_64 := (P and 16#FFFFFFFFFFFFF000#);  -- need to check
      begin
      return Interfaces.C.size_t(PPN);
      end Extract_Physical_Addr;

   begin

   -- add lock here

      if Map.Root = null then
         -- Nothing to do if there's no root table
         Free_Page_Table (Map);
         return;
      end if;

      -- L3 is the top-level array pointed to by Map.Root
      
      for i in Index_Range loop
         declare
            L3_Entry : Unsigned_64 := Map.Root(i);
         begin
            if Is_Valid_Entry(L3_Entry) then
               
               L2_Phys := Extract_Physical_Addr(L3_Entry);

               L2_Virt := To_Address(Memory_Offset + L2_Phys);
               declare
                  L2 : Page_Level
                  with Import, Address => L2_Virt;

               begin
                  -- Walk all 512 entries in L2
                  for j in Index_Range loop
                     if Is_Valid_Entry(L2(j)) then
                        L1_Phys := Extract_Physical_Addr(L2(j));
                        L1_Virt := To_Address(Memory_Offset + L1_Phys);

                        declare
                           L1 : Page_Level
                           with Import, Address => L1_Virt;
                        begin
                           -- Walk all 512 entries in L1
                           for k in Index_Range loop
                              if Is_Valid_Entry(L1(k)) then
                                 -- L1(k) references an actual page frame
                                 Frame_Phys := Extract_Physical_Addr(L1(k));
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
      pragma Unreferenced (Map);
   begin
      return True;
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
