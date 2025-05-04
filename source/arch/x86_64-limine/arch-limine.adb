--  arch-limine.adb: Limine utilities.
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

with System; use System;
with Lib;
with Arch.Debug;  use Arch.Debug;

package body Arch.Limine with SPARK_Mode => Off is   use Arch.Debug;

quest : Limine.Base_Revision :=
      (ID_1     => 16#f9562b2d5c95a6c8#,
       ID_2     => 16#6a7b384944536bdc#,
       Revision => 3)
      with Export, Async_Writers;

   DTB_Response : constant DTB_Response :=
      (Base     => DTB_Request,
       DTB_Addr => System.Null_Address)
      with Import, Address => DTB_Request.Response;

   function Get_Physical_Address return System.Address is
      PhysPonse : Kernel_Address_Response
         with Import, Address => Address_Request.Response;
   begin
      return PhysPonse.Phys_Addr;
   end Get_Physical_Address;

   procedure Translate_Proto is
      InfoPonse : Bootloader_Info_Response
         with Import, Address => Bootloader_Info_Request.Response;
      Name_Addr : constant System.Address := InfoPonse.Name_Addr;
      Name_Len  : constant        Natural := Lib.C_String_Length (Name_Addr);
      Boot_Name : String (1 .. Name_Len) with Import, Address => Name_Addr;
      Vers_Addr : constant System.Address := InfoPonse.Version_Addr;
      Vers_Len  : constant        Natural := Lib.C_String_Length (Vers_Addr);
      Boot_Vers : String (1 .. Vers_Len) with Import, Address => Vers_Addr;

      MemPonse : Memmap_Response
         with Import, Address => Memmap_Request.Response;
      Inner_MMap : constant Memmap_Entry_Arr (1 .. MemPonse.Count)
         with Import, Address => MemPonse.Entries;
      Type_Entry : Boot_Memory_Type;
   begin
      Debug.Print  ("Booted by " & Boot_Name & " " & Boot_Vers);

      if Base_Request.Revision /= 0 then
         Debug.Print  ("The passed revision was not supported!");
      end if;

      declare
         CmdPonse : Kernel_File_Response
            with Import, Address => Kernel_File_Request.Response;
         Cmdline_Addr : constant System.Address :=
            CmdPonse.Kernel_File.Cmdline;
         Cmdline_Len  : constant Natural := Lib.C_String_Length (Cmdline_Addr);
         Cmdline : String (1 .. Cmdline_Len)
            with Import, Address => Cmdline_Addr;
      begin
         Global_Info.Cmdline (1 .. Cmdline_Len) := Cmdline;
         Global_Info.Cmdline_Len := Cmdline_Len;
      end;

      --  Translate RAM files.
      Global_Info.RAM_Files_Len := 0;
      if Modules_Request.Response /= System.Null_Address then
         declare
            ModPonse : Modules_Response
               with Import, Address => Modules_Request.Response;
            Inner_Mods : constant Limine_File_Arr (1 .. ModPonse.Mod_Count)
               with Import, Address => ModPonse.Modules;
         begin
            for Ent of Inner_Mods loop
               Global_Info.RAM_Files_Len := Global_Info.RAM_Files_Len + 1;
               Global_Info.RAM_Files (Global_Info.RAM_Files_Len) :=
                  (Start   => Ent.Address,
                   Length  => Storage_Count (Ent.Size));
            end loop;
         end;
      end if;

      --  Translate memmap.
      for Ent of Inner_MMap loop
         case Ent.EntryType is
            when LIMINE_MEMMAP_USABLE =>
               Type_Entry := Memory_Free;
            when LIMINE_MEMMAP_RESERVED   | LIMINE_MEMMAP_FRAMEBUFFER |
                 LIMINE_MEMMAP_BAD_MEMORY | LIMINE_MEMMAP_BOOTLOADER_RECL =>
               Type_Entry := Memory_Reserved;
            when LIMINE_MEMMAP_ACPI_RECLAIMABLE =>
               Type_Entry := Memory_ACPI_Reclaimable;
            when LIMINE_MEMMAP_ACPI_NVS =>
               Type_Entry := Memory_ACPI_NVS;
            when LIMINE_MEMMAP_KERNEL_AND_MODS =>
               Type_Entry := Memory_Kernel;
            when others =>
               Debug.Print  ("Unrecognized memory assumed reserved");
               Type_Entry := Memory_Reserved;
         end case;

         Global_Info.Memmap_Len := Global_Info.Memmap_Len + 1;
         Global_Info.Memmap (Global_Info.Memmap_Len) :=
            (Start   => To_Address (Integer_Address (Ent.Base)),
             Length  => Storage_Count (Ent.Length),
             MemType => Type_Entry);
      end loop;
   exception
      when Constraint_Error =>
         Debug.Print  ("Exception encountered translating limine");
   end Translate_Proto;

   procedure Initialize_DTB is
      DTB_Response : constant DTB_Response
         with Import, Address => DTB_Request.Response;
   begin
      if DTB_Response.Base.Revision /= 0 or
      else DTB_Response.DTB_Addr = System.Null_Address then
         Debug.Print ("DTB_Response: Invalid response" &
            "(unsupported revision or null address).");
         return;
      end if;

      Debug.Print ("DTB_Response: DTB address = " & Address'Image(DTB_Response.DTB_Addr));
   end Initialize_DTB;
end Arch.Limine;
