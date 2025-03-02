--  lib.ads: Specification of generic library functions.
--  Copyright (C) 2025 streaksu
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

package Lib with Pure is
   --  Get the length of a C-Style string.
   --  @param Addr Address of the C-Style, NUL-terminated string to check.
   --  @return Length of the passed string.
   function C_String_Length (Addr : Address) return Natural
      with Pre => Addr /= System.Null_Address;
end Lib;
