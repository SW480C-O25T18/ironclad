--  devices-partitions.adb: Split a block device into partitions.
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

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;

package body Devices.Partitions with SPARK_Mode => Off is
   --  Data for MBR partitioning.
   type MBR_Byte_Data is array (Natural range <>) of Unsigned_8;
   type MBR_Entry is record
      Status           : Unsigned_8;
      CHS_First_Sector : MBR_Byte_Data (1 .. 3);
      Entry_Type       : Unsigned_8;
      CHS_Last_Sector  : MBR_Byte_Data (1 .. 3);
      First_Sector     : Unsigned_32;
      Sector_Count     : Unsigned_32;
   end record with Size => 128;
   for MBR_Entry use record
      Status           at 0 range  0 ..   7;
      CHS_First_Sector at 0 range  8 ..  31;
      Entry_Type       at 0 range 32 ..  39;
      CHS_Last_Sector  at 0 range 40 ..  63;
      First_Sector     at 0 range 64 ..  95;
      Sector_Count     at 0 range 96 .. 127;
   end record;
   type MBR_Entries is array (1 .. 4) of MBR_Entry;
   type MBR_Data is record
      Boot_Code      : MBR_Byte_Data (1 .. 446);
      Entries        : MBR_Entries;
      Boot_Signature : MBR_Byte_Data (1 .. 2);
   end record with Size => 4096;
   for MBR_Data use record
      Boot_Code      at 0 range    0 .. 3567;
      Entries        at 0 range 3568 .. 4079;
      Boot_Signature at 0 range 4080 .. 4095;
   end record;
   type MBR_Data_Acc is access all MBR_Data;

   --  Data for GPT partitioning.
   type GPT_Header is record
      Signature               : String (1 .. 8);
      Revision                : Unsigned_32;
      Header_Size             : Unsigned_32;
      CRC32                   : Unsigned_32;
      Reserved_0              : Unsigned_32;
      Current_LBA             : Unsigned_64;
      Backup_LBA              : Unsigned_64;
      First_Usable_LBA        : Unsigned_64;
      Last_Usable_LBA         : Unsigned_64;
      GUID                    : Unsigned_128;
      Partition_Entry_LBA     : Unsigned_64;
      Number_Of_Partitions    : Unsigned_32;
      Partition_Entry_Size    : Unsigned_32;
      Partition_Entries_CRC32 : Unsigned_32;
   end record with Size => 736;
   for GPT_Header use record
      Signature               at 0 range   0 ..  63;
      Revision                at 0 range  64 ..  95;
      Header_Size             at 0 range  96 .. 127;
      CRC32                   at 0 range 128 .. 159;
      Reserved_0              at 0 range 160 .. 191;
      Current_LBA             at 0 range 192 .. 255;
      Backup_LBA              at 0 range 256 .. 319;
      First_Usable_LBA        at 0 range 320 .. 383;
      Last_Usable_LBA         at 0 range 384 .. 447;
      GUID                    at 0 range 448 .. 575;
      Partition_Entry_LBA     at 0 range 576 .. 639;
      Number_Of_Partitions    at 0 range 640 .. 671;
      Partition_Entry_Size    at 0 range 672 .. 703;
      Partition_Entries_CRC32 at 0 range 704 .. 735;
   end record;
   type GPT_Header_Acc is access all GPT_Header;
   type GPT_Partition_Entry is record
      Type_GUID      : Unsigned_128;
      Unique_GUID    : Unsigned_128;
      Starting_LBA   : Unsigned_64;
      Ending_LBA     : Unsigned_64;
      Attributes     : Unsigned_64;
      Partition_Name : Wide_String (1 .. 36);
   end record with Size => 1024;
   for GPT_Partition_Entry use record
      Type_GUID      at 0 range   0 ..  127;
      Unique_GUID    at 0 range 128 ..  255;
      Starting_LBA   at 0 range 256 ..  319;
      Ending_LBA     at 0 range 320 ..  383;
      Attributes     at 0 range 384 ..  447;
      Partition_Name at 0 range 448 .. 1023;
   end record;
   type GPT_Entries is array (Natural range <>) of GPT_Partition_Entry;

   --  Datatypes for easing reading sectors up.
   type Sector_Data is array (Unsigned_64 range <>) of Unsigned_8;
   type Sector_Data_Acc is access Sector_Data;
   procedure Free_Sector is new Ada.Unchecked_Deallocation
      (Sector_Data, Sector_Data_Acc);

   --  Packages for conversions.
   package Con1 is new System.Address_To_Access_Conversions (Partition_Data);
   package Con2 is new System.Address_To_Access_Conversions (MBR_Data);
   package Con3 is new System.Address_To_Access_Conversions (GPT_Header);

   function Parse_Partitions
      (Name : String;
       Dev  : Resource_Acc) return Boolean
   is
      Found_Any_Partitions : Boolean;
   begin
      if not Dev.Is_Block or else Dev.Read = null then
         return False;
      end if;

      if not Parse_GPT_Partitions (Name, Dev, Found_Any_Partitions) then
         return False;
      end if;
      if Found_Any_Partitions then
         return True;
      end if;

      return Parse_MBR_Partitions (Name, Dev, Found_Any_Partitions);
   end Parse_Partitions;

   function Parse_GPT_Partitions
      (Name                 : String;
       Dev                  : Resource_Acc;
       Found_Any_Partitions : out Boolean) return Boolean
   is
      Sector : Sector_Data_Acc := new Sector_Data (1 .. Dev.Block_Size);
      S_Addr : constant System.Address := Sector.all'Address;
      GPT    : GPT_Header_Acc;
      Part   : Partition_Data_Acc;

      Parts_Per_Sector : Natural;
      Part_Count       : Natural;
      Block            : Unsigned_64 := 0;
      I                : Natural := 1;
      Added_Index      : Natural := 1;
      Success          : Boolean := True;
   begin
      Found_Any_Partitions := False;
      if Dev.Read (Dev, Dev.Block_Size, Dev.Block_Size, S_Addr) /=
         Dev.Block_Size
      then
         Success := False;
         goto Return_End;
      end if;
      GPT := GPT_Header_Acc (Con3.To_Pointer (S_Addr));

      if GPT.Signature /= "EFI PART" then
         Success := True;
         goto Return_End;
      end if;

      Part_Count       := Natural (GPT.Number_Of_Partitions);
      Block            := Dev.Block_Size * 2;
      Parts_Per_Sector :=
         Natural (Dev.Block_Size / Unsigned_64 (GPT.Partition_Entry_Size / 8));

      loop
         if Dev.Read (Dev, Block, Dev.Block_Size, S_Addr) /= Dev.Block_Size
         then
            Success := False;
            goto Return_End;
         end if;
         declare
            GPT_Parts : GPT_Entries (1 .. Parts_Per_Sector) with
               Import, Address => (S_Addr);
         begin
            for Partition of GPT_Parts loop
               if I >= Part_Count then
                  Success := True;
                  goto Return_End;
               end if;
               if Partition.Type_GUID /= 0 then
                  Found_Any_Partitions := True;
                  Part := new Partition_Data'(
                     Inner_Device => Dev,
                     LBA_Offset   => Partition.Starting_LBA,
                     LBA_Length   => Partition.Ending_LBA -
                                     Partition.Starting_LBA
                  );
                  if not Set_Part (Name, Added_Index, Part) then
                     Success := False;
                     goto Return_End;
                  end if;
                  Added_Index := Added_Index + 1;
               end if;
               I := I + 1;
            end loop;
         end;
         Block := Block + Dev.Block_Size;
      end loop;

   <<Return_End>>
      Free_Sector (Sector);
      return Success;
   end Parse_GPT_Partitions;

   function Parse_MBR_Partitions
      (Name                 : String;
       Dev                  : Resource_Acc;
       Found_Any_Partitions : out Boolean) return Boolean
   is
      Sector  : Sector_Data_Acc := new Sector_Data (1 .. Dev.Block_Size);
      S_Addr  : constant System.Address := Sector.all'Address;
      MBR     : MBR_Data_Acc;
      Part    : Partition_Data_Acc;
      Success : Boolean := True;
   begin
      Found_Any_Partitions := False;
      if Dev.Read (Dev, 0, Dev.Block_Size, S_Addr) /= Dev.Block_Size then
         Success := False;
         goto Return_End;
      end if;
      MBR := MBR_Data_Acc (Con2.To_Pointer (S_Addr));

      if MBR.Boot_Signature /= (16#55#, 16#AA#) then
         Success := True;
         goto Return_End;
      end if;

      for I in MBR.Entries'Range loop
         if MBR.Entries (I).Sector_Count /= 0 then
            Found_Any_Partitions := True;
            Part := new Partition_Data'(
               Inner_Device => Dev,
               LBA_Offset   => Unsigned_64 (MBR.Entries (I).First_Sector),
               LBA_Length   => Unsigned_64 (MBR.Entries (I).Sector_Count)
            );
            if not Set_Part (Name, I, Part) then
               Success := False;
               goto Return_End;
            end if;
         end if;
      end loop;

   <<Return_End>>
      Free_Sector (Sector);
      return Success;
   end Parse_MBR_Partitions;

   function Set_Part
      (Name  : String;
       Index : Positive;
       Part  : Partition_Data_Acc) return Boolean
   is
   begin
      return Register ((
         Data              => Con1.To_Address (Con1.Object_Pointer (Part)),
         Mutex             => Lib.Synchronization.Unlocked_Semaphore,
         Is_Block          => True,
         Block_Size        => Part.Inner_Device.Block_Size,
         Block_Count       => Part.LBA_Length,
         Unique_Identifier => 0,
         Sync              => null,
         Read              => Read'Access,
         Write             => Write'Access,
         IO_Control        => null,
         Mmap              => null,
         Munmap            => null
      ), Name & (1 => 'p', 2 => Character'Val (Index + Character'Pos ('0'))));
   end Set_Part;
   ----------------------------------------------------------------------------
   function Read
      (Data   : Resource_Acc;
       Offset : Unsigned_64;
       Count  : Unsigned_64;
       Desto  : System.Address) return Unsigned_64
   is
      Part : Partition_Data_Acc;
      LBA_Offset, LBA_Length, Final_Count : Unsigned_64;
   begin
      Part       := Partition_Data_Acc (Con1.To_Pointer (Data.Data));
      LBA_Offset := Part.LBA_Offset * Part.Inner_Device.Block_Size;
      LBA_Length := Part.LBA_Length * Part.Inner_Device.Block_Size;

      if Offset > LBA_Offset + LBA_Length then
         return 0;
      elsif Offset + Count > LBA_Offset + LBA_Length then
         Final_Count := LBA_Length - Offset;
      else
         Final_Count := Count;
      end if;

      return Part.Inner_Device.Read
         (Data   => Part.Inner_Device,
          Offset => LBA_Offset + Offset,
          Count  => Final_Count,
          Desto  => Desto);
   end Read;

   function Write
      (Data     : Resource_Acc;
       Offset   : Unsigned_64;
       Count    : Unsigned_64;
       To_Write : System.Address) return Unsigned_64
   is
      Part : Partition_Data_Acc;
      LBA_Offset, LBA_Length, Final_Count : Unsigned_64;
   begin
      Part       := Partition_Data_Acc (Con1.To_Pointer (Data.Data));
      LBA_Offset := Part.LBA_Offset * Part.Inner_Device.Block_Size;
      LBA_Length := Part.LBA_Length * Part.Inner_Device.Block_Size;

      if Offset > LBA_Offset + LBA_Length then
         return 0;
      elsif Offset + Count > LBA_Offset + LBA_Length then
         Final_Count := LBA_Length - Offset;
      else
         Final_Count := Count;
      end if;

      if Part.Inner_Device.Write /= null then
         return Part.Inner_Device.Write
            (Data     => Part.Inner_Device,
             Offset   => LBA_Offset + Offset,
             Count    => Final_Count,
             To_Write => To_Write);
      else
         --  TODO: Return error, not 0.
         return 0;
      end if;
   end Write;
end Devices.Partitions;
