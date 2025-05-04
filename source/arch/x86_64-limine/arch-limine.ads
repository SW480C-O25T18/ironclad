--  arch-limine.ads: Limine utilities.
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

with System;
with Interfaces; use Interfaces;

package Arch.Limine with SPARK_Mode => Off is
   --  Global variable holding platform information.
   Global_Info : Boot_Information;

   --  Get physical address where the kernel is loaded.
   function Get_Physical_Address return System.Address;

   --  Translate a multiboot2 header into architecture info.
   --  @param Proto Pointer to translate, if null, return cached or panic.
   procedure Translate_Proto;
   ----------------------------------------------------------------------------
   Limine_Common_Magic_1 : constant := 16#c7b1dd30df4c8b88#;
   Limine_Common_Magic_2 : constant := 16#0a82e883a194f07b#;

   type DTB_Response is record
      Base     : Response;
      DTB_Addr : System.Address;
   end record with Pack;

   -- Response is a pointer to a DTB_Response.
   DTB_Request : Request :=
      (ID       => DTB_ID,
      Revision => 0,
      Response => System.Null_Address)
      with Export, Async_Writers;

private

   --  Here lie requests that all limine architectures will depend on, and
   --  are used internally for this module.

   --  Response is a pointer to a Bootloader_Info_Response.
   Bootloader_Info_Request : Request :=
      (ID       => Bootloader_Info_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  Response is a pointer to a Kernel_File_Response.
   Kernel_File_Request : Request :=
      (ID       => Kernel_File_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  Response is a pointer to a Memmap_Response.
   Memmap_Request : Request :=
      (ID       => Memmap_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  Response is a pointer to an Kernel_Address_Response.
   Address_Request : Request :=
      (ID       => Kernel_Address_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;

   --  Response is a pointer to an Modules_Response.
   Modules_Request : Request :=
      (ID       => Modules_ID,
       Revision => 0,
       Response => System.Null_Address)
      with Export, Async_Writers;
end Arch.Limine;
