--  devices-streams.ads: Virtual stream device library specification.
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

with Interfaces; use Interfaces;
with System;
with VFS; use VFS;

package Devices.Streams is
   --  Initialize the device.
   function Init return Boolean;

private

   function Nulldev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : Unsigned_64;
       Count  : Positive;
       Desto  : System.Address) return Natural;

   function Nulldev_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : Unsigned_64;
       Count    : Positive;
       To_Write : System.Address) return Natural;
   ----------------------------------------------------------------------------
   function Zerodev_Read
      (Data   : Root_Data;
       Obj    : Object;
       Offset : Unsigned_64;
       Count  : Positive;
       Desto  : System.Address) return Natural;

   function Zerodev_Write
      (Data     : Root_Data;
       Obj      : Object;
       Offset   : Unsigned_64;
       Count    : Positive;
       To_Write : System.Address) return Natural;
end Devices.Streams;
