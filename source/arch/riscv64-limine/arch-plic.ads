--  arch-plic.ads: Specification of Platform-Level Interrupt Controller
--  (PLIC) utilities.
--  Provides an interface for configuring and managing the PLIC on
--  RISC-V64 systems.
--  Handles interrupt enabling, priority management, and
--  context-specific configurations.
--  Fully compliant with the RISC-V PLIC v1.0/v1.1 specifications.
--  Copyright (C) 2025 Sean C. Weeks
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
with System.Storage_Elements;  use System.Storage_Elements;
with Arch.DTB;                 use Arch.DTB;
with Arch.MMU;                 use Arch.MMU;

package Arch.PLIC with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------
   --  Identifier for a context (Machine or Supervisor)
   type Context_Type is (Machine_Mode, Supervisor_Mode);
   type Context_Id is new Context_Type;

   ---------------------------------------------------------------------------
   --  Global Variables (post-configuration)
   ---------------------------------------------------------------------------
   --  Base virtual address of the PLIC region
   Plic_Base : System.Address;

   --  Byte-offset of PLIC region from its base address
   Plic_Base_Off : Storage_Offset;

   --  Number of interrupt sources supported
   Num_Sources : Unsigned_32;

   --  Number of contexts (2 × hart count)
   Num_Contexts : Unsigned_64;

   --  Detected PLIC version (1.0, 1.1, etc.)
   Plic_Version : Unsigned_32;

   ---------------------------------------------------------------------------
   --  Constants for PLIC register offsets
   ---------------------------------------------------------------------------
   -- Base offset for priority registers
   Priority_Base : constant Storage_Offset := 16#0000#;

   -- Base offset for enable bits
   Enable_Base : constant Storage_Offset := 16#2000#;

   -- Base offset for claim/complete registers
   Claim_Base : constant Storage_Offset := 16#200004#;

   -- Stride for context-specific regions
   Context_Stride : constant Storage_Offset := 16#1000#;

   ---------------------------------------------------------------------------
   --  Interrupt Priority and Threshold Constants
   ---------------------------------------------------------------------------
   -- Minimum and maximum interrupt priority levels
   Min_Priority : constant Unsigned_32 := 0;
   Max_Priority : constant Unsigned_32 := 7;

   -- Default interrupt threshold values
   Default_Threshold : constant Unsigned_32 := 0;

   ---------------------------------------------------------------------------
   --  Context-Specific Constants
   ---------------------------------------------------------------------------
   -- Context-specific offsets
   Machine_Context_Offset : constant Storage_Offset := 0;
   Supervisor_Context_Offset : constant Storage_Offset := Context_Stride;

   -- Supported PLIC versions
   PLIC_Version_1_0 : constant Unsigned_32 := 16#10#;
   PLIC_Version_1_1 : constant Unsigned_32 := 16#11#;

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
   -- Initialize the PLIC with default values
   procedure Initialize_PLIC;
   
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
     pragma Inline_Always;

   --  Calculate the machine-mode context index for a hart.
   function Machine_Context (Hart : Unsigned_64)
     return Context_Id;
       pragma Inline_Always;

private

   ---------------------------------------------------------------------------
   --  PLIC Register Offsets (bytes)
   ---------------------------------------------------------------------------
   --  Base for priority registers: Plic_Base + Priority_Base + 4×IRQ_Id
   Priority_Base : constant Storage_Offset := 0;

   --  Step size between priority registers (in bytes)
   Priority_Step : constant Storage_Offset := 4;

   --  Enable bits start at Plic_Base + Enable_Base + Context_Stride×Ctx
   Enable_Base : constant Storage_Offset := 16#2000#;

   --  Per-context stride for enable, threshold, and claim regions
   Context_Stride : constant Storage_Offset := 16#1000#;

   --  Threshold register at Plic_Base +
   --  Threshold_Base + Context_Stride×Ctx
   Threshold_Base : constant Storage_Offset := 16#200000#;

   --  Claim/Complete register at Plic_Base +
   --  Threshold_Base + 4 + Context_Stride×Ctx
   Claim_Base : constant Storage_Offset := 16#200004#;

   -- Page size for MMIO mapping
   Page_Size : constant Storage_Offset := 16#1000#;

end Arch.PLIC;