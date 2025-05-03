--  arch-sbi.ads: Specification of Supervisor Binary Interface (SBI) utilities.
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
with System;     --  for System.Address

package Arch.SBI with SPARK_Mode => Off is

   ------------------------------------------------------------------------
   --  Types
   ------------------------------------------------------------------------
   type Extension_Id is new Unsigned_64;
     --  Encodes the SBI extension ID (legacy or string-based).

   ------------------------------------------------------------------------
   --  Probe Extension
   ------------------------------------------------------------------------
   function Probe_Extension
     (Ext : Extension_Id) return Boolean;
     --  Returns True if the specified SBI extension is supported.

   ------------------------------------------------------------------------
   --  Timer Management
   ------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64);
     --  Requests a timer interrupt at the given absolute time.

   ------------------------------------------------------------------------
   --  Inter-Processor Interrupts (IPIs)
   ------------------------------------------------------------------------
   procedure Send_Ipi (Hart_Mask_Ptr : System.Address);
     --  Sends software interrupts to harts specified in hart-mask.

   ------------------------------------------------------------------------
   --  Remote Fence (e.g., TLB shootdown)
   ------------------------------------------------------------------------
   procedure Remote_Fence (Hart_Mask_Ptr : System.Address);
     --  Sends a remote fence IPI to harts specified in hart-mask.

end Arch.SBI;
