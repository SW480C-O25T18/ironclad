--  arch-clint.ads: Specification of Core Local Interruptor (CLINT) utilities.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program. If not,
--  see [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

with Interfaces;                     use Interfaces;
with System;                         use System;

package Arch.CLINT with SPARK_Mode => Off is

   --  Availability: prefer SBI virtualization, fallback to MMIO
   function CLINT_Enabled return Boolean;

   -----------------------------------------------------------
   --  Global configuration: map MMIO or rely on SBI
   -----------------------------------------------------------
   procedure Set_CLINT_Configuration;

   -----------------------------------------------------------
   --  Monotonic timer frequency (ticks per second)
   --  Initialized from the DTB "timebase-frequency" property
   -----------------------------------------------------------

   Timebase_Frequency : Unsigned_64;

   --  Read the current timer (mtime CSR)
   function Read_Timer return Unsigned_64;

   --  Program the next timer interrupt (mtimecmp register)
   procedure Set_Timer (Next_Time : Unsigned_64);

   --  Software local interrupts (IPIs)
   procedure Send_Software_Interrupt (Target_Hart : Unsigned_64);
   procedure Clear_Software_Interrupt (Hart_Id      : Unsigned_64);

   --  Remote fence IPI (for SBI fence-IPI extension)
   procedure Send_Fence_Ipi (Target_Hart : Unsigned_64);

   --  Per-hart initialization (e.g., clear MSIP)
   procedure Initialize_Hart (Hart_Id : Unsigned_64);

   -----------------------------------------------------------
   --  Supervisor-mode timer interrupt enable/disable
   --  (manipulates SIE.STIE bit)
   -----------------------------------------------------------
   procedure Disable_Timer_Interrupt;
   procedure Enable_Timer_Interrupt;

   function Reg_Addr (Off : Storage_Offset) return Address;

end Arch.CLINT;
