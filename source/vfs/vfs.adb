--  vfs.adb: FS and register dispatching.
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

with VFS.USTAR;
with System; use System;
with Lib.Synchronization;
with Ada.Unchecked_Deallocation;
with Lib.Alignment;

package body VFS with SPARK_Mode => Off is
   type Sector_Data is array (Unsigned_64 range <>) of Unsigned_8;
   type Sector_Cache (Size : Unsigned_64) is record
      LBA_Offset : Unsigned_64;
      Data       : Sector_Data (1 .. Size);
   end record;
   type Sector_Cache_Acc is access Sector_Cache;
   procedure Free_Sector_Cache is new Ada.Unchecked_Deallocation
      (Sector_Cache, Sector_Cache_Acc);
   type Sector_Cache_Arr is array (Unsigned_64 range <>) of Sector_Cache_Acc;
   Path_Buffer_Length : constant :=  100;
   Cache_Array_Length : constant := 8000;
   type Mount_Container is record
      Mounted_Dev : Devices.Resource_Acc;
      Mounted_FS  : FS_Type;
      FS_Data     : System.Address;
      Path_Length : Natural;
      Path_Buffer : String (1 .. Path_Buffer_Length);
      Cache_Mutex : aliased Lib.Synchronization.Binary_Semaphore;
      Cache       : Sector_Cache_Arr (1 .. Cache_Array_Length);
      Last_Evict  : Unsigned_64;
   end record;
   type Mount_Container_Arr is array (1 .. 5) of Mount_Container;
   Mounts       : access Mount_Container_Arr;
   Mounts_Mutex : aliased Lib.Synchronization.Binary_Semaphore;

   package Ali is new Lib.Alignment (Unsigned_64);

   procedure Init is
   begin
      Mounts       := new Mount_Container_Arr;
      Mounts_Mutex := Lib.Synchronization.Unlocked_Semaphore;
      for Mount of Mounts.all loop
         Mount.Mounted_Dev := null;
         Mount.Cache       := (others => null);
         Mount.Cache_Mutex := Lib.Synchronization.Unlocked_Semaphore;
         Mount.Last_Evict  := 1;
      end loop;
   end Init;

   function Mount (Name, Path : String; FS : FS_Type) return Boolean is
      Dev     : constant Devices.Resource_Acc := Devices.Fetch (Name);
      FS_Data : System.Address := System.Null_Address;
      Free_I  : Natural        := 0;
   begin
      if not Is_Absolute (Path) or Path'Length > Path_Buffer_Length or
         Dev = null
      then
         return False;
      end if;

      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev = Dev then
            goto Return_End;
         elsif Mounts (I).Mounted_Dev = null then
            Free_I := I;
         end if;
      end loop;
      if Free_I = 0 then
         goto Return_End;
      end if;

      --  We need an initialized mount for probing the FS, thats why the
      --  awkward split initialization.
      Mounts (Free_I).Mounted_Dev                    := Dev;
      Mounts (Free_I).Path_Length                    := Path'Length;
      Mounts (Free_I).Path_Buffer (1 .. Path'Length) := Path;
      case FS is
         when FS_USTAR =>
            FS_Data := VFS.USTAR.Probe (Free_I);
            if FS_Data /= System.Null_Address then
               Mounts (Free_I).Mounted_FS := FS_USTAR;
               Mounts (Free_I).FS_Data    := FS_Data;
            else
               Free_I := 0;
            end if;
      end case;

   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Free_I /= 0;
   end Mount;

   procedure Unmount (Path : String) is
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= null and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            Flush_Caches (I);
            Mounts (I).Mounted_Dev := null;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
   end Unmount;

   function Get_Mount (Path : String) return Natural is
      Returned : Natural := 0;
   begin
      Lib.Synchronization.Seize (Mounts_Mutex);
      for I in Mounts'Range loop
         if Mounts (I).Mounted_Dev /= null and then
            Mounts (I).Path_Buffer (1 .. Mounts (I).Path_Length) = Path
         then
            Returned := I;
            goto Return_End;
         end if;
      end loop;
   <<Return_End>>
      Lib.Synchronization.Release (Mounts_Mutex);
      return Returned;
   end Get_Mount;

   function Is_Valid (Key : Positive) return Boolean is
   begin
      return Key <= Mounts'Last and then Mounts (Key).Mounted_Dev /= null;
   end Is_Valid;

   function Get_Backing_FS (Key : Positive) return FS_Type is
   begin
      return Mounts (Key).Mounted_FS;
   end Get_Backing_FS;

   function Get_Backing_FS_Data (Key : Positive) return System.Address is
   begin
      return Mounts (Key).FS_Data;
   end Get_Backing_FS_Data;

   function Get_Backing_Device (Key : Positive) return Devices.Resource_Acc is
   begin
      return Mounts (Key).Mounted_Dev;
   end Get_Backing_Device;

   function Read
      (Key    : Positive;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Data : Sector_Data (1 .. Count) with Import, Address => Desto;
      Block_Size : constant Unsigned_64 := Mounts (Key).Mounted_Dev.Block_Size;
      Ali_Offset : constant Unsigned_64 := Ali.Align_Down (Offset, Block_Size);
      Low_LBA    : constant Unsigned_64 := Ali_Offset / Block_Size;
      Ali_Length : constant Unsigned_64 :=
         Ali.Align_Up (Count + Offset - Ali_Offset, Block_Size);
      LBA_Count : constant Unsigned_64 := Ali_Length / Block_Size;
      Caches : Sector_Cache_Arr (1 .. LBA_Count) := (others => null);
      Free_Index     : Unsigned_64;
      Sector_Index   : Unsigned_64;
      Initial_Offset : Unsigned_64;
      Searched       : Unsigned_64;
   begin
      if Count = 0 then
         return 0;
      end if;

      Lib.Synchronization.Seize (Mounts (Key).Cache_Mutex);
      for I in 1 .. LBA_Count loop
         Free_Index := 0;
         Searched   := Low_LBA + I - 1;
         for J in Mounts (Key).Cache'Range loop
            if Mounts (Key).Cache (J) = null then
               if Free_Index = 0 then
                  Free_Index := J;
               end if;
            elsif Mounts (Key).Cache (J).LBA_Offset = Searched then
               Caches (I) := Mounts (Key).Cache (J);
               goto Found;
            end if;
         end loop;
         if Free_Index = 0 then
            Free_Index := Mounts (Key).Last_Evict;
            Evict_Sector
               (Mounts (Key).Mounted_Dev,
                Mounts (Key).Cache (Free_Index).LBA_Offset,
                Searched,
                Mounts (Key).Cache (Free_Index).Data'Address);
            Mounts (Key).Cache (Free_Index).LBA_Offset := Searched;
            Caches (I) := Mounts (Key).Cache (Free_Index);
            if Mounts (Key).Last_Evict = Cache_Array_Length then
               Mounts (Key).Last_Evict := 1;
            else
               Mounts (Key).Last_Evict := Mounts (Key).Last_Evict + 1;
            end if;
         else
            Mounts (Key).Cache (Free_Index) := new Sector_Cache (Block_Size);
            Mounts (Key).Cache (Free_Index).LBA_Offset := Searched;
            if not Read_Sector
               (Dev   => Mounts (Key).Mounted_Dev,
                LBA   => Searched,
                Desto => Mounts (Key).Cache (Free_Index).Data'Address)
            then
               return 0;
            end if;
            Caches (I) := Mounts (Key).Cache (Free_Index);
         end if;
      <<Found>>
      end loop;

      Free_Index     := 1;
      Sector_Index   := 1;
      Initial_Offset := Offset - Ali_Offset;
      for Byte of Data loop
         Byte := Caches (Sector_Index).Data (Initial_Offset + Free_Index);
         if Free_Index >= Block_Size - Initial_Offset then
            Free_Index     := 1;
            Sector_Index   := Sector_Index + 1;
            Initial_Offset := 0;
         else
            Free_Index := Free_Index + 1;
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts (Key).Cache_Mutex);
      return Count;
   end Read;

   function Write
      (Key      : Positive;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
   begin
      return Mounts (Key).Mounted_Dev.Write
         (Data     => Mounts (Key).Mounted_Dev,
          Offset   => Offset,
          Count    => Count,
          To_Write => To_Write);
   end Write;

   procedure Evict_Sector
      (Dev              : Devices.Resource_Acc;
       Old_LBA, New_LBA : Unsigned_64;
       Data             : System.Address)
   is
      Discard : Boolean;
   begin
      Discard := Write_Sector
         (Dev  => Dev,
          LBA  => Old_LBA,
          Data => Data);
      Discard := Read_Sector
         (Dev   => Dev,
          LBA   => New_LBA,
          Desto => Data);
   end Evict_Sector;

   procedure Flush_Caches (Key : Positive) is
      Discard : Boolean;
   begin
      Lib.Synchronization.Seize (Mounts (Key).Cache_Mutex);
      for Cache of Mounts (Key).Cache loop
         if Cache /= null then
            Discard := Write_Sector
               (Dev  => Mounts (Key).Mounted_Dev,
                LBA  => Cache.LBA_Offset,
                Data => Cache.Data'Address);
            Free_Sector_Cache (Cache);
         end if;
      end loop;
      Lib.Synchronization.Release (Mounts (Key).Cache_Mutex);
   end Flush_Caches;

   procedure Flush_Caches is
   begin
      for I in Mounts'Range loop
         Flush_Caches (I);
      end loop;
   end Flush_Caches;

   function Read_Sector
      (Dev   : Devices.Resource_Acc;
       LBA   : Unsigned_64;
       Desto : System.Address) return Boolean
   is
   begin
      if Dev.Read = null then
         return False;
      end if;

      return Dev.Read.all
         (Data   => Dev,
          Offset => LBA * Dev.Block_Size,
          Count  => Dev.Block_Size,
          Desto  => Desto) = Dev.Block_Size;
   end Read_Sector;

   function Write_Sector
      (Dev  : Devices.Resource_Acc;
       LBA  : Unsigned_64;
       Data : System.Address) return Boolean
   is
   begin
      if Dev.Write = null then
         return False;
      end if;

      return Dev.Write.all
         (Data     => Dev,
          Offset   => LBA * Dev.Block_Size,
          Count    => Dev.Block_Size,
          To_Write => Data) = Dev.Block_Size;
   end Write_Sector;
   ----------------------------------------------------------------------------
   function Is_Absolute (Path : String) return Boolean is
   begin
      return Path'Length >= 1 and then Path (Path'First) = '/';
   end Is_Absolute;

   function Is_Canonical (Path : String) return Boolean is
      Previous : Character := ' ';
   begin
      if not Is_Absolute (Path) or else Path (Path'Last) = '/' then
         return False;
      end if;

      for C of Path loop
         if (Previous = '/' and C = '/') or (Previous = '.' and C = '.') then
            return False;
         end if;
         Previous := C;
      end loop;

      return True;
   end Is_Canonical;
end VFS;
