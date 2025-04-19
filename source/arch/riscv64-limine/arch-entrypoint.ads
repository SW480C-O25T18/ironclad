--  arch-entrypoint.ads: Limine plops us here.
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

with Interfaces; use Interfaces;

package Arch.Entrypoint is
   --  Convert Unsigned_64 to String using Ada.Strings.Fixed
   subtype Context_String is String (1 .. 1020);
   function Unsigned_To_String (Value : Unsigned_64) return Context_String;

   --  Consolidate Exception Handling to reduce code size
   --  and improve readability.
   procedure Handle_Exception (Context : Context_String);

   --  This is the entry point for the kernel. It is called by Limine
   --  after the bootloader has loaded the kernel into memory.
   procedure Bootstrap_Main
      with Export, Convention => C, External_Name => "kernel_main";
end Arch.Entrypoint;
