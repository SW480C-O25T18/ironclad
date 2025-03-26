--  arch-interrupts.ads: Specification of interrupt utilities for RISC-V64 architecture.
--  Provides support for dynamic IRQ registration, dispatching, and device IRQ handling.
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

with Interfaces; use Interfaces;
with System;           -- For System.Address
with Arch.DTB;         -- For DTB node access

package Arch.Interrupts with SPARK_Mode => Off is
   ---------------------------------------------------------------------------
   -- Types and Constants
   ---------------------------------------------------------------------------

   -- Type for IRQ handler procedures
   type IRQ_Handler is access procedure;

   -- Maximum number of IRQs supported
   Max_IRQs : constant Integer := 255;

   -- Frame type for interrupt handling (architecture-specific)
   type Frame is record
      -- Caller-Saved Registers:
      x1_ra   : Unsigned_64;  -- x1 (ra): Return address (caller-saved)
      x4_tp   : Unsigned_64;  -- x4 (tp): Thread pointer (volatile)
      x5_t0   : Unsigned_64;  -- x5 (t0): Temporary (caller-saved)
      x6_t1   : Unsigned_64;  -- x6 (t1): Temporary (caller-saved)
      x7_t2   : Unsigned_64;  -- x7 (t2): Temporary (caller-saved)
      x10_a0  : Unsigned_64;  -- x10 (a0): Function argument/return (caller-saved)
      x11_a1  : Unsigned_64;  -- x11 (a1): Function argument/return (caller-saved)
      x12_a2  : Unsigned_64;  -- x12 (a2): Function argument (caller-saved)
      x13_a3  : Unsigned_64;  -- x13 (a3): Function argument (caller-saved)
      x14_a4  : Unsigned_64;  -- x14 (a4): Function argument (caller-saved)
      x15_a5  : Unsigned_64;  -- x15 (a5): Function argument (caller-saved)
      x16_a6  : Unsigned_64;  -- x16 (a6): Function argument (caller-saved)
      x17_a7  : Unsigned_64;  -- x17 (a7): Function argument / syscall number (caller-saved)
      x28_t3  : Unsigned_64;  -- x28 (t3): Temporary (caller-saved)
      x29_t4  : Unsigned_64;  -- x29 (t4): Temporary (caller-saved)
      x30_t5  : Unsigned_64;  -- x30 (t5): Temporary (caller-saved)
      x31_t6  : Unsigned_64;  -- x31 (t6): Temporary (caller-saved)

      -- Callee-Saved Registers:
      x2_sp   : Unsigned_64;  -- x2 (sp): Stack pointer (callee-saved)
      x3_gp   : Unsigned_64;  -- x3 (gp): Global pointer (callee-saved)
      x8_s0   : Unsigned_64;  -- x8 (s0/fp): Saved register / frame pointer (callee-saved)
      x9_s1   : Unsigned_64;  -- x9 (s1): Saved register (callee-saved)
      x18_s2  : Unsigned_64;  -- x18 (s2): Saved register (callee-saved)
      x19_s3  : Unsigned_64;  -- x19 (s3): Saved register (callee-saved)
      x20_s4  : Unsigned_64;  -- x20 (s4): Saved register (callee-saved)
      x21_s5  : Unsigned_64;  -- x21 (s5): Saved register (callee-saved)
      x22_s6  : Unsigned_64;  -- x22 (s6): Saved register (callee-saved)
      x23_s7  : Unsigned_64;  -- x23 (s7): Saved register (callee-saved)
      x24_s8  : Unsigned_64;  -- x24 (s8): Saved register (callee-saved)
      x25_s9  : Unsigned_64;  -- x25 (s9): Saved register (callee-saved)
      x26_s10 : Unsigned_64;  -- x26 (s10): Saved register (callee-saved)
      x27_s11 : Unsigned_64;  -- x27 (s11): Saved register (callee-saved)

      -- Control/Status Registers:
      sepc    : Unsigned_64;  -- Exception program counter (resume address)
      scause  : Unsigned_64;  -- Trap cause (provided by hardware)
      stval   : Unsigned_64;  -- Trap value (e.g., faulting address)
      sstatus : Unsigned_64;  -- Supervisor status register (includes SPP bit)

      -- Floating-Point Context Pointer (for lazy FP state saving)
      FP_Context_Ptr : System.Address := System.Null_Address;
   end record with Pack;

   ---------------------------------------------------------------------------
   -- IRQ Registration and Dispatching
   ---------------------------------------------------------------------------

   -- Register an IRQ handler for a specific IRQ number
   -- Raises Constraint_Error if the IRQ number is invalid
   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler);

   -- Unregister an IRQ handler for a specific IRQ number
   -- Raises Constraint_Error if the IRQ number is invalid
   procedure Unregister_IRQ (IRQ : Integer);

   -- Dispatch an interrupt to the appropriate handler
   -- This is called by the interrupt controller when an IRQ occurs
   procedure Handle_Interrupt (Frame_Ptr : in out Frame);

   ---------------------------------------------------------------------------
   -- Device IRQ Registration
   ---------------------------------------------------------------------------

   -- Register a device's IRQ handler based on its DTB node
   -- Uses the "interrupts" property from the DTB node to determine the IRQ
   procedure Register_Device_IRQ (Node : access Arch.DTB.DTB_Node; Handler : IRQ_Handler);

   ---------------------------------------------------------------------------
   -- Floating-Point Context Management
   ---------------------------------------------------------------------------

   -- Save the floating-point context during an interrupt
   procedure Save_FP_Context (Frame_Ptr : in out Frame);

   -- Restore the floating-point context after an interrupt
   procedure Restore_FP_Context (Frame_Ptr : in out Frame);

end Arch.Interrupts;