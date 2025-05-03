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
with System;           --  for System.Address

package Arch.Interrupts with SPARK_Mode => Off is

   ------------------------------------------------------------------------
   --  Configuration Constants (compile-time selection)
   ------------------------------------------------------------------------
   --  Set to True if Ada.Tasking and Protected Objects are available
   Use_Protected_Registry : constant Boolean := False;

   --  Set to True to attempt vectored interrupt dispatch
   Use_Vectored_Dispatch   : constant Boolean := False;

   ------------------------------------------------------------------------
   --  Types & Limits
   ------------------------------------------------------------------------
   type IRQ_Handler is access procedure;
   Max_Cause    : constant Unsigned_64 := 255;

   ------------------------------------------------------------------------
   --  Trap Frame Layout (packed to match assembly stub)
   ------------------------------------------------------------------------
   type Frame is record
      --  Caller-saved registers
      x1_ra        : Unsigned_64;
      x4_tp        : Unsigned_64;
      x5_t0        : Unsigned_64;
      x6_t1        : Unsigned_64;
      x7_t2        : Unsigned_64;
      x10_a0       : Unsigned_64;
      x11_a1       : Unsigned_64;
      x12_a2       : Unsigned_64;
      x13_a3       : Unsigned_64;
      x14_a4       : Unsigned_64;
      x15_a5       : Unsigned_64;
      x16_a6       : Unsigned_64;
      x17_a7       : Unsigned_64;
      x28_t3       : Unsigned_64;
      x29_t4       : Unsigned_64;
      x30_t5       : Unsigned_64;
      x31_t6       : Unsigned_64;

      --  Callee-saved registers
      x2_sp        : Unsigned_64;
      x3_gp        : Unsigned_64;
      x8_s0        : Unsigned_64;
      x9_s1        : Unsigned_64;
      x18_s2       : Unsigned_64;
      x19_s3       : Unsigned_64;
      x20_s4       : Unsigned_64;
      x21_s5       : Unsigned_64;
      x22_s6       : Unsigned_64;
      x23_s7       : Unsigned_64;
      x24_s8       : Unsigned_64;
      x25_s9       : Unsigned_64;
      x26_s10      : Unsigned_64;
      x27_s11      : Unsigned_64;

      --  Control/status registers
      sepc         : Unsigned_64;
      scause       : Unsigned_64;
      stval        : Unsigned_64;
      sstatus      : Unsigned_64;

      --  Stack pointers for user/kernel switch
      User_SP      : Unsigned_64;
      Ker_SP       : Unsigned_64;

      --  Supervisor CSR snapshots
      sie          : Unsigned_64;
      sip          : Unsigned_64;

      --  Lazy-FP context pointer
      FP_Context_Ptr : System.Address := System.Null_Address;
   end record
   with Pack;

   --  Ensure 8-byte alignment and correct frame size (320 bytes)
   for Frame'Alignment use 8;
   pragma Assert (Frame'Size = 320);

   ------------------------------------------------------------------------
   --  Initialization
   ------------------------------------------------------------------------
   function Get_TCP_OFFSET return Unsigned_64;
   procedure Initialize;

   ------------------------------------------------------------------------
   --  Unified Handler Registration
   ------------------------------------------------------------------------
   --  Install or remove a handler for any trap/interrupt cause.
   procedure Install_Handler
     (Is_Interrupt : Boolean;
      Cause        : Unsigned_64;
      Handler      : IRQ_Handler);

   procedure Remove_Handler
     (Is_Interrupt : Boolean;
      Cause        : Unsigned_64);

   ------------------------------------------------------------------------
   --  Trap entry/exit (assembler stubs)
   ------------------------------------------------------------------------
   procedure trap_entry;
   pragma Import (Assembler, trap_entry, "trap_entry");

   procedure trap_exit;
   pragma Import (Assembler, trap_exit,  "trap_exit");

   ------------------------------------------------------------------------
   --  Core Dispatchers
   ------------------------------------------------------------------------
   procedure Handle_Interrupt (Frame_Ptr : in out Frame);
   procedure Handle_Trap      (Frame_Ptr : access Frame);

   ------------------------------------------------------------------------
   --  Floating-point context (lazy)
   ------------------------------------------------------------------------
   procedure Save_FP_Context    (Frame_Ptr : in out Frame);
   procedure Restore_FP_Context (Frame_Ptr : in out Frame);

end Arch.Interrupts;