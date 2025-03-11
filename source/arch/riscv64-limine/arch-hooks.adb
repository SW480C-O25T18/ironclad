--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

with Devices.UART;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
--with Arch.APIC;
with Arch.Interrupts;
with Interfaces; use Interfaces;
with Lib.Messages;
with Devices.Ramdev;
with Arch.Limine;

package body Arch.Hooks is
   function Devices_Hook return Boolean is
   begin
      return Devices.UART.Init_UART0;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      pragma Unreferenced (Code);
      pragma Unreferenced (Arg);
   begin
      return True;
   end PRCTL_Hook;

   procedure Panic_SMP_Hook is
   begin
      null;
      Lib.Messages.Put_Line ("Panic: System Halted");
   end Panic_SMP_Hook;

   function Get_Active_Core_Count return Positive is
   begin
      return Core_Count;
   end Get_Active_Core_Count;

   procedure Register_RAM_Files is
   begin
      if not Devices.Ramdev.Init
         (Limine.Global_Info.RAM_Files (1 .. Limine.Global_Info.RAM_Files_Len))
      then
         Lib.Messages.Put_Line ("Could not load RAM files");
      end if;
   exception
      when Constraint_Error =>
         Lib.Messages.Put_Line ("Errored while loading RAM files");
   end Register_RAM_Files;
end Arch.Hooks;
