--  arch-plic.ads: Specification of Platform-Level Interrupt Controller
--  (PLIC) utilities.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
--  Based on RISC-V PLIC v1.0/v1.1 specifications
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

with Interfaces;                      use Interfaces;
with System;                          use System;
with System.Storage_Elements;         use System.Storage_Elements;
with Arch.DTB;                        use Arch.DTB;
with Arch.MMU;                        use Arch.MMU;

package Arch.PLIC with SPARK_Mode => Off is

   ------------------------------------------------------------------------
   --  Global Variables
   ------------------------------------------------------------------------
   Plic_Base      : Address      := Null_Address;
     --  Base virtual address of the PLIC MMIO region.

   Plic_Base_Off  : Storage_Offset := 0;
     --  Cached integer offset of Plic_Base for rapid MMIO math.

   Num_Sources    : Unsigned_32  := 0;
     --  Number of interrupt source IDs supported by this PLIC.

   Num_Contexts   : Unsigned_64  := 0;
     --  Number of hart+privilege contexts supported by this PLIC.

   Plic_Version   : Unsigned_32  := 1;
     --  PLIC version detected (fallback to 1.0 if unspecified).

   ------------------------------------------------------------------------
   --  PLIC register offsets
   ------------------------------------------------------------------------
   Priority_Base  : constant Storage_Offset := Storage_Offset (16#0000#);
     --  Priority registers base.
   Pending_Base   : constant Storage_Offset := Storage_Offset (16#1000#);
     --  Pending bits base (unused by this kernel).
   Enable_Base    : constant Storage_Offset := Storage_Offset (16#2000#);
     --  Enable bits base, per context.
   Threshold_Base : constant Storage_Offset := Storage_Offset (16#200000#);
     --  Threshold register base, per context.
   Claim_Base     : constant Storage_Offset := Storage_Offset (16#200004#);
     --  Claim/complete register base, per context.
   Context_Stride : constant Storage_Offset := Storage_Offset (16#100#);
     --  Stride between context-specific register blocks.

   ------------------------------------------------------------------------
   --  Types
   ------------------------------------------------------------------------
   type IRQ_Id     is new Unsigned_64;
     --  PLIC interrupt source identifier.

   type Context_Id is new Unsigned_64;
     --  PLIC context identifier (hart + privilege level).

   ------------------------------------------------------------------------
   --  Context Helpers
   ------------------------------------------------------------------------
   function Supervisor_Context (Hart : Unsigned_64) return Context_Id;
     --  Compute S-mode context ID (hart*2 + 1).

   function Machine_Context (Hart : Unsigned_64) return Context_Id;
     --  Compute M-mode context ID (hart*2 + 0).

   ------------------------------------------------------------------------
   --  PLIC availability
   ------------------------------------------------------------------------
   function Is_Enabled return Boolean;
     --  True if a compatible PLIC node is present in the DTB.

   ------------------------------------------------------------------------
   --  Global configuration
   ------------------------------------------------------------------------
   procedure Set_PLIC_Configuration;
     --  Map the PLIC MMIO window, initialize globals and detect version.

   ------------------------------------------------------------------------
   --  Hart-specific initialization
   ------------------------------------------------------------------------
   procedure Initialize (
     Hart_Id : Unsigned_64;
     Ctx     : Context_Id
   );
     --  Set threshold to zero and enable IRQs for this context.

   ------------------------------------------------------------------------
   --  Interrupt claim and completion
   ------------------------------------------------------------------------
   function Claim (
     Hart_Id : Unsigned_64;
     Ctx     : Context_Id
   ) return IRQ_Id;
     --  Claim the next pending interrupt ID for this context.

   procedure Complete (
     Hart_Id : Unsigned_64;
     Ctx     : Context_Id;
     IRQ     : IRQ_Id
   );
     --  Signal completion of a claimed interrupt.

   ------------------------------------------------------------------------
   --  Source Configuration
   ------------------------------------------------------------------------
   procedure Set_Priority (
     Id       : IRQ_Id;
     Priority : Unsigned_32
   );
     --  Set the priority for interrupt source Id.

   procedure Enable_IRQ (
     Hart_Id : Unsigned_64;
     Ctx     : Context_Id;
     Id      : IRQ_Id
   );
     --  Unmask the given interrupt source for this context.

   procedure Disable_IRQ (
     Hart_Id : Unsigned_64;
     Ctx     : Context_Id;
     Id      : IRQ_Id
   );
     --  Mask the given interrupt source for this context.

end Arch.PLIC;