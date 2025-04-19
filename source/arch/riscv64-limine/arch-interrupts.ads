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
with System;           --  For System.Address
with Arch.DTB;         --  For DTB node access

package Arch.Interrupts with SPARK_Mode => Off is
   ---------------------------------------------------------------------------
   --  Types and Constants
   ---------------------------------------------------------------------------

   --  Type for IRQ handler procedures
   type IRQ_Handler is access procedure;

   --  Maximum number of IRQs supported
   Max_IRQs : constant Integer := 255;

   --  Frame type for interrupt handling (architecture-specific)
   type Frame is record
      --  Caller-Saved Registers:
      x1_ra   : Unsigned_64;  --  x1 (ra): Return address (caller-saved) @ 0
      x4_tp   : Unsigned_64;  --  x4 (tp): Thread pointer (volatile) @ 8
      x5_t0   : Unsigned_64;  --  x5 (t0): Temporary (caller-saved) @ 16
      x6_t1   : Unsigned_64;  --  x6 (t1): Temporary (caller-saved) @ 24
      x7_t2   : Unsigned_64;  --  x7 (t2): Temporary (caller-saved) @ 32
      x10_a0  : Unsigned_64;  --  x10 (a0): Function argument/return (caller-saved) @ 40
      x11_a1  : Unsigned_64;  --  x11 (a1): Function argument/return (caller-saved) @ 48
      x12_a2  : Unsigned_64;  --  x12 (a2): Function argument (caller-saved) @ 56
      x13_a3  : Unsigned_64;  --  x13 (a3): Function argument (caller-saved) @ 64
      x14_a4  : Unsigned_64;  --  x14 (a4): Function argument (caller-saved) @ 72
      x15_a5  : Unsigned_64;  --  x15 (a5): Function argument (caller-saved) @ 80
      x16_a6  : Unsigned_64;  --  x16 (a6): Function argument (caller-saved) @ 88
      x17_a7  : Unsigned_64;  --  x17 (a7): Function argument / syscall number (caller-saved) @ 96
      x28_t3  : Unsigned_64;  --  x28 (t3): Temporary (caller-saved) @ 104
      x29_t4  : Unsigned_64;  --  x29 (t4): Temporary (caller-saved) @ 112
      x30_t5  : Unsigned_64;  --  x30 (t5): Temporary (caller-saved) @ 120
      x31_t6  : Unsigned_64;  --  x31 (t6): Temporary (caller-saved) @ 128

      --  Callee-Saved Registers:
      x2_sp   : Unsigned_64;  --  x2 (sp): Stack pointer (callee-saved) @ 136
      x3_gp   : Unsigned_64;  --  x3 (gp): Global pointer (callee-saved) @ 144
      x8_s0   : Unsigned_64;  --  x8 (s0/fp): Saved register / frame pointer (callee-saved) @ 152
      x9_s1   : Unsigned_64;  --  x9 (s1): Saved register (callee-saved) @ 160
      x18_s2  : Unsigned_64;  --  x18 (s2): Saved register (callee-saved) @ 168
      x19_s3  : Unsigned_64;  --  x19 (s3): Saved register (callee-saved) @ 176
      x20_s4  : Unsigned_64;  --  x20 (s4): Saved register (callee-saved) @ 184
      x21_s5  : Unsigned_64;  --  x21 (s5): Saved register (callee-saved) @ 192
      x22_s6  : Unsigned_64;  --  x22 (s6): Saved register (callee-saved) @ 200
      x23_s7  : Unsigned_64;  --  x23 (s7): Saved register (callee-saved) @ 208
      x24_s8  : Unsigned_64;  --  x24 (s8): Saved register (callee-saved) @ 216
      x25_s9  : Unsigned_64;  --  x25 (s9): Saved register (callee-saved) @ 224
      x26_s10 : Unsigned_64;  --  x26 (s10): Saved register (callee-saved) @ 232
      x27_s11 : Unsigned_64;  --  x27 (s11): Saved register (callee-saved) @ 240

      --  Control/Status Registers:
      sepc    : Unsigned_64;  --  Exception program counter (resume address) @ 248
      scause  : Unsigned_64;  --  Trap cause (provided by hardware) @ 256
      stval   : Unsigned_64;  --  Trap value (e.g., faulting address) @ 264
      sstatus : Unsigned_64;  --  Supervisor status register (includes SPP bit) @ 272

      --  Floating-Point Context Pointer (for lazy FP state saving)
      FP_Context_Ptr : System.Address := System.Null_Address; --  @ 280
   end record
   with Pack;

   --  pragma Assert (Frame.x1_ra'Position       = 0);
   --  pragma Assert (Frame.x4_tp'Position       = 8);
   --  pragma Assert (Frame.x5_t0'Position       = 16);
   --  pragma Assert (Frame.x17_a7'Position      = 96);
   --  pragma Assert (Frame.x31_t6'Position      = 128);
   --  pragma Assert (Frame.x3_gp'Position       = 144);
   --  pragma Assert (Frame.x27_s11'Position     = 240);
   --  pragma Assert (Frame.sepc'Position        = 248);
   --  pragma Assert (Frame.scause'Position      = 256);
   --  pragma Assert (Frame.sstatus'Position     = 272);
   --  pragma Assert (Frame.FP_Context_Ptr'Position = 280);

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------
   procedure Initialize;

   ---------------------------------------------------------------------------
   --  IRQ Registration and Dispatching
   ---------------------------------------------------------------------------
   procedure Register_IRQ (IRQ : Integer; Handler : IRQ_Handler);
   procedure Unregister_IRQ (IRQ : Integer);

   ---------------------------------------------------------------------------
   --  Interrupt Handling
   ---------------------------------------------------------------------------
   --  Called by the hardware on any trap/interrupt
   procedure trap_entry;
   pragma Import (Assembler, trap_entry, "trap_entry");
   --  Return path from trap
   procedure trap_exit;
   pragma Import (Assembler, trap_exit,  "trap_exit");
   procedure Handle_Interrupt (Frame_Ptr : in out Frame);
   procedure Handle_Trap (Frame_Ptr : access Frame);

   ---------------------------------------------------------------------------
   --  Device IRQ Registration
   ---------------------------------------------------------------------------
   procedure Register_Device_IRQ (
      Node : access Arch.DTB.DTB_Node_Access; Handler : IRQ_Handler);

   ---------------------------------------------------------------------------
   --  Floating-Point Context Management
   ---------------------------------------------------------------------------
   procedure Save_FP_Context (Frame_Ptr : in out Frame);
   procedure Restore_FP_Context (Frame_Ptr : in out Frame);

end Arch.Interrupts;