--  devices-ata.ads: ATA driver.
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

package Devices.ATA with SPARK_Mode => Off is
   --  Probe for ATA drives and add em.
   function Init return Boolean;

private

   --  ATA drives come in pairs, master and slave, and usually found in one of
   --  these 2 ports.
   Pair_Ports : constant array (0 .. 1) of Unsigned_16 := (16#1F0#, 16#170#);

   --  Data stored for each drive.
   Sector_Size : constant := 512;
   subtype Sector_Data is Operation_Data (1 .. Sector_Size);
   type Sector_Cache is record
      Is_Used    : Boolean;
      LBA_Offset : Unsigned_64;
      Is_Dirty   : Boolean;
      Data       : Sector_Data;
   end record;
   type Sector_Caches is array (Natural range <>) of Sector_Cache;

   type ATA_Identify is array (1 .. 256) of Unsigned_16;
   type ATA_Data is record
      Is_Master     : Boolean;
      Identify      : ATA_Identify;
      Data_Port     : Unsigned_16;
      Error_Port    : Unsigned_16;
      Count_Port    : Unsigned_16;
      LBA_Low_Port  : Unsigned_16;
      LBA_Mid_Port  : Unsigned_16;
      LBA_High_Port : Unsigned_16;
      Device_Port   : Unsigned_16;
      Command_Port  : Unsigned_16;
      Control_Port  : Unsigned_16;
      Sector_Count  : Unsigned_64;
      Caches        : Sector_Caches (1 .. 8000);
      Next_Evict    : Natural range 1 .. 8000;
   end record;
   type ATA_Data_Acc is access all ATA_Data;

   --  Probe a port and return an initialized ATA drive, or null if not found.
   function Init_Port (Port_Index : Natural) return ATA_Data_Acc;

   --  Read a single sector.
   function Read_Sector
      (Drive       : ATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : out Sector_Data) return Boolean;

   --  Write a single sector.
   function Write_Sector
      (Drive       : ATA_Data_Acc;
       LBA         : Unsigned_64;
       Data_Buffer : Sector_Data) return Boolean;

   --  Issue an LBA command.
   function Issue_Command
      (Drive : ATA_Data_Acc;
       LBA   : Unsigned_64;
       Cmd   : Unsigned_8) return Boolean;

   --  Poll for ATA comamnd error.
   function Poll_Error (Port : Unsigned_16) return Boolean;

   --  Find a cache index for the drive given the desired LBA.
   function Get_Cache_Index
      (Drive : ATA_Data_Acc;
       LBA   : Unsigned_64) return Natural;
   ----------------------------------------------------------------------------
   procedure Read
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);

   procedure Write
      (Key       : Resource_Acc;
       Offset    : Unsigned_64;
       Data      : Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean);

   procedure Sync (Key : Resource_Acc);
end Devices.ATA;
