--  arch-exceptions.adb: Package body of interrupt utilities.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
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
with System;           -- For System.Address
with Arch.Snippets;     -- For low-level interrupt enabling/disabling
with Arch.PLIC;         -- For handling the interrupt controller
with Arch.Context;      -- For FP and general context management
with Arch.Interrupts;   -- For the interrupt frame type

package body Arch.Interrupts with SPARK_Mode => Off is

procedure Handle_Interrupt (Frame_Ptr : in out Frame) is
begin 
   null;
end Handle_Interrupt;

procedure Save_FP_Context (Frame_Ptr : in out Frame) is
begin 
   null;
end Save_FP_Context;

procedure Restore_FP_Context (Frame_Ptr : in out Frame) is
begin 
   null;
end Restore_FP_Context;

end Arch.Interrupts;