--  arch-clocks.adb: Architectural clock sources.
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

package body Arch.Clocks with
   Refined_State =>
      (RT_Clock_State =>
         (RT_Timestamp_Seconds,
          RT_Timestamp_Nanoseconds,
          RT_Stored_Seconds,
          RT_Stored_Nanoseconds),
       Monotonic_Clock_State => (Mono_TSC_Freq))
is
   RT_Timestamp_Seconds     : Unsigned_64;
   RT_Timestamp_Nanoseconds : Unsigned_64;
   RT_Stored_Seconds        : Unsigned_64;
   RT_Stored_Nanoseconds    : Unsigned_64;

   MS_Per_Sec    : constant := 1_000;
   Nanos_Per_MS  : constant := 1_000_000;
   Mono_TSC_Freq : Unsigned_64 := Nanos_Per_MS * 2;

   pragma Unreferenced (
     RT_Timestamp_Seconds,
     RT_Timestamp_Nanoseconds,
     RT_Stored_Seconds,
     RT_Stored_Nanoseconds,
     MS_Per_Sec,
     Mono_TSC_Freq
   );

   procedure Initialize_Sources is
   begin
      null;
   end Initialize_Sources;

   procedure Get_Monotonic_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Monotonic_Resolution;

   procedure Get_Monotonic_Time (Seconds, Nanoseconds : out Unsigned_64) is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Monotonic_Time;

   procedure Get_Real_Time_Resolution (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Real_Time_Resolution;

   procedure Get_Real_Time (Seconds, Nanoseconds : out Unsigned_64)
   is
   begin
      Seconds     := 0;
      Nanoseconds := 0;
   end Get_Real_Time;

   procedure Set_Real_Time (Seconds, Nanoseconds : Unsigned_64) is
   begin
      null;
   end Set_Real_Time;
end Arch.Clocks;
