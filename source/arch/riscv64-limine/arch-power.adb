--  -- filepath: /home/crew18/Gloire/ironclad/source/arch/riscv64-limine/arch-power.adb
--  --  arch-power.adb: Architecture-specific power management for RISC-V with Limine.
--  --  Copyright (C) 2025 streaksu
--  --
--  --  This program is free software: you can redistribute it and/or modify
--  --  it under the terms of the GNU General Public License as published by
--  --  the Free Software Foundation, either version 3 of the License, or
--  --  (at your option) any later version.
--  --
--  --  This program is distributed in the hope that it will be useful,
--  --  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  --  GNU General Public License for more details.
--  --
--  --  You should have received a copy of the GNU General Public License
--  --  along with this program.  If not, see <http://www.gnu.org/licenses/>.

--  with Arch.Snippets;
--  with Lib.Panic;
--  with SBI;  -- Include SBI interface for system reset
--  with Ada.Text_IO;  -- For terminal output

--  package body Arch.Power is

--     procedure Halt (Status : out Power_Status) is
--     begin
--        --  Halt the system by entering an infinite loop.
--        Lib.Panic.Hard_Panic ("System halted.");
--        Ada.Text_IO.Put_Line ("Halt procedure called: System is halting.");

--        -- Uncomment the following line to simulate a failure state for testing.
--        -- Status := Failure;
--        -- Ada.Text_IO.Put_Line ("Halt procedure: Simulated failure state.");
--        -- return;

--        loop
--           null;
--        end loop;

--        --  If somehow we exit the loop, set the status to failure.
--        Status := Failure;
--        lib.Messages.Put_Line ("Halt procedure: Exited loop unexpectedly. Status set to Failure.");
--     end Halt;

--     procedure Reboot (Status : out Power_Status) is
--        Result : Sbiret;
--     begin
--        lib.Messages.Put_Line ("Reboot procedure called: Attempting to reboot.");
      
--        loop 
--           null;
         
--        end loop;
--        -- Uncomment the following line to simulate a failure state for testing.
--        -- Status := Failure;
--        -- Ada.Text_IO.Put_Line ("Reboot procedure: Simulated failure state.");
--        -- return;

--        --  Perform a cold reboot using SBI system reset.
--        Result := SBI.System_Reset (16#00000001#, 16#00000000#); -- Cold reboot, no reason

--        --  Check for errors in the SBI response
--        if Result.Error /= 1 then
--           Status := Failure;
--           Ada.Text_IO.Put_Line ("Reboot procedure: SBI call failed. Status set to Failure.");
--        else
--           -- Uncomment the following line to force a success state for testing.
--           -- Status := Success;
--           -- Ada.Text_IO.Put_Line ("Reboot procedure: Simulated success state.");
--           -- return;

--           -- This point should never be reached if reboot succeeds
--           Status := Success;
--           lib.Messages.Put_Line ("Reboot procedure: SBI call succeeded. Status set to Success.");
--        end if;
--    end Reboot;

--     procedure Poweroff (Status : out Power_Status) is
--        Result : Sbiret;
--     begin
--        Ada.Text_IO.Put_Line ("Poweroff procedure called: Attempting to power off.");

--        -- Uncomment the following line to simulate a failure state for testing.
--        -- Status := Failure;
--        -- Ada.Text_IO.Put_Line ("Poweroff procedure: Simulated failure state.");
--        -- return;

--        --  Attempt to power off using SBI system reset.
--        Result := SBI.System_Reset (16#00000000#, 16#00000000#); -- Shutdown, no reason

--        --  Check for errors in the SBI response
--        if Result.Error /= 0 then
--           Status := Failure;
--           Ada.Text_IO.Put_Line ("Poweroff procedure: SBI call failed. Status set to Failure.");
--        else
--           -- Uncomment the following line to force a success state for testing.
--           -- Status := Success;
--           -- Ada.Text_IO.Put_Line ("Poweroff procedure: Simulated success state.");
--           -- return;

--           -- This point should never be reached if poweroff succeeds
--           Status := Success;
--           Ada.Text_IO.Put_Line ("Poweroff procedure: SBI call succeeded. Status set to Success.");
--        end if;
--     end Poweroff;

--  end Arch.Power;
--  arch-power.adb: Architecture-specific power management.
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

package body Arch.Power is
   procedure Halt (Status : out Power_Status) is
   begin
      Status := Failure;
   end Halt;

   procedure Reboot (Status : out Power_Status) is
   begin
      Status := Failure;
   end Reboot;

   procedure Poweroff (Status : out Power_Status) is
   begin
      Status := Failure;
   end Poweroff;
end Arch.Power;