--  arch-exceptions.ads: Specification of interrupt utilities.
--  Copyright (C) 2021 streaksu
--  Updated the Interrupts package specification to match the riscv64 ISA.
--  This package provides the Frame type, which represents the complete CPU 
--  state saved by the low-level trap entry code when an interrupt or exception occurs. 
--  It captures all general-purpose registers (excluding x0, which is always zero) along 
--  with the key control/status registers. The package also provides the Handle_Interrupt procedure, 
--  which is invoked by the low-level trap entry (in assembly). It receives a pointer to a 
--  Frame that holds the saved CPU state. The handler can then examine the state to dispatch system calls, 
--  handle exceptions, or perform context switching. The package also provides Save_FP_Context 
--  and Restore_FP_Context procedures, which are invoked only when the current task has used FP instructions. 
--  They call into Arch.Context to save or restore the FP state (f0–f31), and update the 
--  FP_Context_Ptr field in the Frame accordingly.
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

package Arch.Interrupts with SPARK_Mode => Off is
   ------------------------------------------------------------------------------
   --  Interrupt Frame for RISCV64
   --
   --  This type represents the complete CPU state saved by the low-level trap entry
   --  code when an interrupt or exception occurs. It captures all general-purpose
   --  registers (excluding x0, which is always zero) along with the key control/
   --  status registers.
   --
   --  Register Organization:
   --
   --    Caller-Saved Registers (17 total):
   --      x1  (ra)   : Return address
   --      x4  (tp)   : Thread pointer (volatile)
   --      x5  (t0)   : Temporary
   --      x6  (t1)   : Temporary
   --      x7  (t2)   : Temporary
   --      x10 (a0)   : Function argument/return value
   --      x11 (a1)   : Function argument/return value
   --      x12 (a2)   : Function argument
   --      x13 (a3)   : Function argument
   --      x14 (a4)   : Function argument
   --      x15 (a5)   : Function argument
   --      x16 (a6)   : Function argument
   --      x17 (a7)   : Function argument / syscall number
   --      x28 (t3)   : Temporary
   --      x29 (t4)   : Temporary
   --      x30 (t5)   : Temporary
   --      x31 (t6)   : Temporary
   --
   --    Callee-Saved Registers (14 total):
   --      x2  (sp)   : Stack pointer
   --      x3  (gp)   : Global pointer
   --      x8  (s0/fp): Saved register / frame pointer
   --      x9  (s1)   : Saved register
   --      x18 (s2)   : Saved register
   --      x19 (s3)   : Saved register
   --      x20 (s4)   : Saved register
   --      x21 (s5)   : Saved register
   --      x22 (s6)   : Saved register
   --      x23 (s7)   : Saved register
   --      x24 (s8)   : Saved register
   --      x25 (s9)   : Saved register
   --      x26 (s10)  : Saved register
   --      x27 (s11)  : Saved register
   --
   --    Control/Status Registers:
   --      sepc    : Exception program counter (resume address)
   --      scause  : Trap cause (provided by hardware)
   --      stval   : Trap value (e.g., faulting address)
   --      sstatus : Supervisor status register (includes the SPP bit)
   --
   --    Floating-Point Context Pointer:
   --      FP_Context_Ptr : Points to a separately saved FP context (f0–f31).
   --                       It is managed by Arch.Context. When a task first uses FP
   --                       instructions, Arch.Context initializes the FP context and
   --                       populates this pointer.
   ------------------------------------------------------------------------------
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

   ------------------------------------------------------------------------------
   --  Constant Definitions
   --
   --  SSTATUS_SPP: Bit in sstatus that indicates the previous privilege level.
   --             (SPP = 0 for user mode, SPP = 1 for kernel mode.)
   ------------------------------------------------------------------------------
   SSTATUS_SPP : constant Unsigned_64 := 2 ** 8;

   ------------------------------------------------------------------------------
   --  High-Level Trap/Interrupt Handling Routine
   --
   --  This procedure is invoked by the low-level trap entry (in assembly). It receives
   --  a pointer to a Frame that holds the saved CPU state. The handler can then examine
   --  the state to dispatch system calls, handle exceptions, or perform context switching.
   ------------------------------------------------------------------------------
   procedure Handle_Interrupt (Frame_Ptr : in out Frame);

   ------------------------------------------------------------------------------
   --  Floating-Point Context Management (Lazy Saving)
   --
   --  These routines are invoked only when the current task has used FP instructions.
   --  They call into Arch.Context to save or restore the FP state (f0–f31), and update
   --  the FP_Context_Ptr field in the Frame accordingly.
   ------------------------------------------------------------------------------
   procedure Save_FP_Context (Frame_Ptr : in out Frame);
   procedure Restore_FP_Context (Frame_Ptr : in out Frame);

end Arch.Interrupts;
