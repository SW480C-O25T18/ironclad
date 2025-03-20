-- filepath: /home/crew18/Gloire/ironclad/source/arch/riscv64-limine/arch-power.adb
--  arch-power.adb: Architecture-specific power management for RISC-V with Limine.
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

with Arch.Snippets;
with Lib.Panic;

package body Arch.Power is
   procedure Halt (Status : out Power_Status) is
   begin
      --  Halt the system by entering an infinite loop.
      Lib.Panic.Hard_Panic ("System halted.");
      loop
         null;
      end loop;

      --  If somehow we exit the loop, set the status to failure.
      Status := Failure;
   end Halt;

   procedure Reboot (Status : out Power_Status) is
   begin
      --  Attempt to reboot using a RISC-V-specific mechanism.
      --  Write to the machine-mode reset CSR (Control and Status Register).
      Snippets.CSR_Write (16#330#, 16#1#); -- Example: Writing to a reset CSR.

      --  If reboot fails, set the status to failure.
      Status := Failure;
   end Reboot;

   procedure Poweroff (Status : out Power_Status) is
   begin
      --  Attempt to power off using a RISC-V-specific mechanism.
      --  Write to a power-off CSR or use a specific memory-mapped I/O address.
      Snippets.CSR_Write (16#331#, 16#0#); -- Example: Writing to a power-off CSR.

      --  If poweroff fails, set the status to failure.
      Status := Failure;
   end Poweroff;
end Arch.Power;
