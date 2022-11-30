--  devices.adb: Device management.
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

with Devices.Random;
with Devices.Streams;
with Devices.Debug;
with Lib.Panic;
with Arch.Hooks;
with Devices.PTY;
with Config;

package body Devices with SPARK_Mode => Off is
   procedure Init is
   begin
      --  Initialize architectural devices.
      Arch.Hooks.Devices_Hook;

      --  Initialize common devices.
      if not Random.Init or not Debug.Init or not Streams.Init then
         goto Failure;
      end if;

      --  Initialize devices for non small builds.
      if not Config.Is_Small then
         if not PTY.Init then
            goto Failure;
         end if;
      end if;

      return;

   <<Failure>>
      Lib.Panic.Hard_Panic ("Could not start arch-independent devices");
   end Init;
end Devices;
