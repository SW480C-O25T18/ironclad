--  vfs-file.ads: File creation and management.
--  Copyright (C) 2021 streaksu
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

package VFS.File is
   type Access_Mode is (Access_R, Access_W, Access_RW);
   type File is record
      Root   : VFS.Root;
      Object : VFS.Object;
      Index  : Natural;
      Flags  : Access_Mode;
   end record;
   type File_Acc is access all File;

   --  Open a file with an absolute path, and return it, or null on failure.
   function Open (Path : String; Access_Flags : Access_Mode) return File_Acc;

   --  Close an opened file.
   procedure Close (To_Close : File_Acc);

   --  Read from a file, and return the read count.
   function Read
      (To_Read     : File_Acc;
       Count       : Natural;
       Destination : System.Address) return Natural;

   --  Write to a file, and return the written count.
   function Write
      (To_Write : File_Acc;
       Count    : Natural;
       Data     : System.Address) return Natural;

   --  Get the stat of the file.
   function Stat (F : File_Acc; S : out VFS.File_Stat) return Boolean;
end VFS.File;
