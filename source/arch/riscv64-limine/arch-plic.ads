--  arch-plic.ads: Specification of Platform-Level Interrupt Controller (PLIC) utilities.
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

with Interfaces;               use Interfaces;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with Arch.DTB;                 use Arch.DTB;
with Arch.MMU;                 use Arch.MMU;
with Arch.Debug;               use Arch.Debug;
with Arch.CPU;                 use Arch.CPU;

package Arch.PLIC with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------
   --  Identifier for an interrupt source
   type IRQ_Id     is new Unsigned_32;

   --  Identifier for a context (Machine or Supervisor)
   type Context_Id is new Unsigned_32;

   ---------------------------------------------------------------------------
   --  Global Variables (post-configuration)
   ---------------------------------------------------------------------------
   --  Base virtual address of the PLIC region
   Plic_Base     : System.Address;
   --  Byte-offset of PLIC region from its base address
   Plic_Base_Off : Storage_Offset;
   --  Number of interrupt sources supported
   Num_Sources   : Unsigned_32;
   --  Number of contexts (2 × hart count)
   Num_Contexts  : Unsigned_64;
   --  Detected PLIC version (1.0, 1.1, etc.)
   Plic_Version  : Unsigned_32;

   ---------------------------------------------------------------------------
   --  Presence Detection
   ---------------------------------------------------------------------------
   --  True if a 'riscv,plic0' node exists and can be mapped
   function Is_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------
   --  Maps the PLIC MMIO region, caches offsets, and reads #interrupts,
   --  contexts, and version from the DTB.
   procedure Set_PLIC_Configuration;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------
   --  Enable all interrupts (threshold=0) for the given context.
   procedure Initialize
     (Hart_Id : Unsigned_64;
      Ctx     : Context_Id);

   ---------------------------------------------------------------------------
   --  Threshold Tuning
   ---------------------------------------------------------------------------
   --  Dynamically adjust the priority threshold for a context (0–max).
   procedure Set_Threshold
     (Ctx       : Context_Id;
      Threshold : Unsigned_32);

   ---------------------------------------------------------------------------
   --  Priority Management
   ---------------------------------------------------------------------------
   --  Set the priority for a given interrupt source.
   procedure Set_Priority
     (Id       : IRQ_Id;
      Priority : Unsigned_32);

   ---------------------------------------------------------------------------
   --  Mask/Unmask Interrupts
   ---------------------------------------------------------------------------
   --  Enable delivery of a specific IRQ in a context.
   procedure Enable_IRQ
     (Hart_Id : Unsigned_64;
      Ctx     : Context_Id;
      Id      : IRQ_Id);

   --  Disable delivery of a specific IRQ in a context.
   procedure Disable_IRQ
     (Hart_Id : Unsigned_64;
      Ctx     : Context_Id;
      Id      : IRQ_Id);

   ---------------------------------------------------------------------------
   --  Claim & Complete
   ---------------------------------------------------------------------------
   --  Claim returns the next pending IRQ ID (or 0 if none).
   function Claim
     (Hart_Id : Unsigned_64;
      Ctx     : Context_Id)
     return IRQ_Id;

   --  Signal completion of a handled IRQ.
   procedure Complete
     (Hart_Id : Unsigned_64;
      Ctx     : Context_Id;
      IRQ     : IRQ_Id);

   ---------------------------------------------------------------------------
   --  Context Convenience
   ---------------------------------------------------------------------------
   --  Calculate the supervisor-mode context index for a hart.
   function Supervisor_Context (Hart : Unsigned_64)
     return Context_Id;

   --  Calculate the machine-mode context index for a hart.
   function Machine_Context (Hart : Unsigned_64)
     return Context_Id;

private

   ---------------------------------------------------------------------------
   --  PLIC Register Offsets (bytes)
   ---------------------------------------------------------------------------
   --  Base for priority registers: Plic_Base + Priority_Base + 4×IRQ_Id
   Priority_Base   : constant Storage_Offset := 0;
   Priority_Step   : constant Storage_Offset := 4;

   --  Enable bits start at Plic_Base + Enable_Base + Context_Stride×Ctx
   Enable_Base     : constant Storage_Offset := 16#2000#;

   --  Per-context stride for enable, threshold, and claim regions
   Context_Stride  : constant Storage_Offset := 16#1000#;

   --  Threshold register at Plic_Base + Threshold_Base + Context_Stride×Ctx
   Threshold_Base  : constant Storage_Offset := 16#200000#;

   --  Claim/Complete register at Plic_Base + Threshold_Base + 4 + Context_Stride×Ctx
   Claim_Base      : constant Storage_Offset := 16#200004#;

end Arch.PLIC;