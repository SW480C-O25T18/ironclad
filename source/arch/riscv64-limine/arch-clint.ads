--  arch-clint.ads: Specification of Core Local Interruptor (CLINT) utilities.
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

with Interfaces;                use Interfaces;
with System;                    use System;
with System.Storage_Elements;   use System.Storage_Elements;
with Arch.SBI;                  use Arch.SBI;
with Arch.MMU;                  use Arch.MMU;
with Arch.DTB;                  use Arch.DTB;

package Arch.CLINT with SPARK_Mode => Off is

   ------------------------------------------------------------------------
   --  Global Variables
   ------------------------------------------------------------------------
   Clint_Base     : Address        := Null_Address;
     --  Virtual base address of CLINT MMIO (fallback).

   Clint_Base_Off : Storage_Offset := 0;
     --  Cached integer offset of Clint_Base for MMIO arithmetic.

   ------------------------------------------------------------------------
   --  CLINT register offsets
   ------------------------------------------------------------------------
   MSIP_Base      : constant Storage_Offset := Storage_Offset (16#0000#);
     --  Software interrupt register base.
   MTIMECMP_Base  : constant Storage_Offset := Storage_Offset (16#4000#);
     --  Timer compare register base.
   MTIMECMP_Step  : constant Storage_Offset := Storage_Offset (8);
     --  Stride between MTIMECMP entries per hart.

   ------------------------------------------------------------------------
   --  CLINT availability
   ------------------------------------------------------------------------
   function CLINT_Enabled return Boolean;
     --  True if SBI timer extension available or MMIO base is set.

   ------------------------------------------------------------------------
   --  Global configuration
   ------------------------------------------------------------------------
   procedure Set_CLINT_Configuration;
     --  Initialize CLINT: prefer SBI extensions, fallback to MMIO mapping.

   ------------------------------------------------------------------------
   --  Hart-specific initialization
   ------------------------------------------------------------------------
   procedure Initialize_Hart (Hart_Id : Unsigned_64);
     --  Clear software interrupt (MSIP) for the given hart.

   ------------------------------------------------------------------------
   --  Software IPIs
   ------------------------------------------------------------------------
   procedure Send_Software_Interrupt (Target_Hart : Unsigned_64);
     --  Trigger software interrupt to Target_Hart.

   procedure Clear_Software_Interrupt (Hart_Id : Unsigned_64);
     --  Clear software interrupt for Hart_Id.

   procedure Send_Fence_Ipi (Target_Hart : Unsigned_64);
     --  Send a remote fence-IPI, or fallback to software IPI.

   ------------------------------------------------------------------------
   --  Timer interrupts
   ------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64);
     --  Set the next timer interrupt (SBI or MMIO MTIMECMP).

   procedure Clear_Timer_Interrupt (Hart_Id : Unsigned_64);
     --  No-op on RISC-V for clearing timer interrupt.

   procedure Disable_Timer_Interrupt;
     --  Mask supervisor timer interrupts (clear STIE).

   procedure Enable_Timer_Interrupt;
     --  Unmask supervisor timer interrupts (set STIE).

end Arch.CLINT;