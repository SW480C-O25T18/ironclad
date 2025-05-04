--  arch-interrupts.ads: Specification of interrupt utilities for RISC-V64 architecture.
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

with Interfaces;        use Interfaces;
with System;            use System;
with Arch.CLINT;      use Arch.CLINT;
with Arch.PLIC;       use Arch.PLIC;
with Arch.DTB;        use Arch.DTB;

package Arch.Interrupts with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Symbolic scause codes and interrupt bit mask
   ---------------------------------------------------------------------------
   Interrupt_Bit         : constant Unsigned_64 :=
                              Interfaces.Shift_Left (Unsigned_64 (1), 63);
   CLINT_SW_INT_CODE     : constant Unsigned_64 := Interrupt_Bit + 3;
   CLINT_TIMER_INT_CODE1 : constant Unsigned_64 := Interrupt_Bit + 5;
   CLINT_TIMER_INT_CODE2 : constant Unsigned_64 := Interrupt_Bit + 7;
   SYSCALL_INT_CODE      : constant Unsigned_64 := 8;  -- exception (ecall)
   PLIC_EXT_INT_CODE1    : constant Unsigned_64 := Interrupt_Bit + 9;
   PLIC_EXT_INT_CODE2    : constant Unsigned_64 := Interrupt_Bit + 11;

   ---------------------------------------------------------------------------
   --  Decode Functions
   ---------------------------------------------------------------------------
   function Cause_Code (S : Unsigned_64) return Unsigned_64;
   pragma Inline (Cause_Code);
   --  Extract cause number by clearing interrupt bit.

   function Is_Interrupt (S : Unsigned_64) return Boolean;
   pragma Inline (Is_Interrupt);
   --  True if scause indicates an interrupt.

   ---------------------------------------------------------------------------
   --  Thread Control Block context offset (bytes)
   ---------------------------------------------------------------------------
   TCB_CONTEXT_OFFSET : constant Natural := 72;
   function Get_TCB_OFFSET return Unsigned_64;
   pragma Inline (Get_TCB_OFFSET);
   --  Byte offset of saved Frame in the TCB.

   ---------------------------------------------------------------------------
   --  Trap Vector Mode Configuration
   ---------------------------------------------------------------------------
   procedure Configure_Trap_Vector;
   --  Detect and enable vectored vs direct trap mode at runtime.

   ---------------------------------------------------------------------------
   --  Interrupt Frame Layout
   ---------------------------------------------------------------------------
   type Frame is record
      --  Caller-saved registers
      x1_ra,  x4_tp,  x5_t0,  x6_t1,  x7_t2   : Unsigned_64;
      x10_a0, x11_a1, x12_a2, x13_a3           : Unsigned_64;
      x14_a4, x15_a5, x16_a6, x17_a7           : Unsigned_64;
      x28_t3, x29_t4, x30_t5, x31_t6           : Unsigned_64;
      --  Callee-saved registers
      x2_sp, x3_gp, x8_s0, x9_s1               : Unsigned_64;
      x18_s2, x19_s3, x20_s4, x21_s5           : Unsigned_64;
      x22_s6, x23_s7, x24_s8, x25_s9           : Unsigned_64;
      x26_s10, x27_s11                         : Unsigned_64;
      --  Control/Status registers
      sepc, scause, stval, sstatus             : Unsigned_64;
      --  Lazy floating-point context pointer
      FP_Context_Ptr : System.Address := System.Null_Address;
   end record with Pack;

   ---------------------------------------------------------------------------
   --  IRQ Handlers Table
   ---------------------------------------------------------------------------
   type IRQ_Handler is access procedure;
   Max_IRQs : constant Integer := 255;
   type IRQ_Table_Type is array (0 .. Max_IRQs) of IRQ_Handler;
   IRQ_Table  : IRQ_Table_Type := (others => null);
   IRQ_Counts : array (0 .. Max_IRQs) of Natural := (others => 0);

   ---------------------------------------------------------------------------
   --  IRQ Dispatch Metrics
   ---------------------------------------------------------------------------
   function Get_IRQ_Count (IRQ : Integer) return Natural
     with Pre => IRQ in IRQ_Table'Range;
   --  Return number of times IRQ has been dispatched.

   procedure Reset_IRQ_Count (IRQ : Integer)
     with Pre => IRQ in IRQ_Table'Range;
   --  Reset dispatch count for IRQ to zero.

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------
   procedure Initialize
     with Post => (CLINT_Enabled or PLIC.Is_Enabled);
   --  Setup CLINT/PLIC and configure trap mode.

   ---------------------------------------------------------------------------
   --  Dynamic IRQ Registration
   ---------------------------------------------------------------------------
   procedure Register_IRQ
     (IRQ     : Integer;
      Handler : IRQ_Handler)
     with Pre => IRQ in IRQ_Table'Range and Handler /= null;
   --  Register a handler for IRQ, atomic with interrupts disabled.

   procedure Unregister_IRQ
     (IRQ : Integer)
     with Pre => IRQ in IRQ_Table'Range;
   --  Unregister handler for IRQ.

   ---------------------------------------------------------------------------
   --  Trap Entry/Exit (Assembler Stubs)
   ---------------------------------------------------------------------------
   procedure trap_entry;
   pragma Import (Assembler, trap_entry, "trap_entry");

   procedure trap_exit;
   pragma Import (Assembler, trap_exit, "trap_exit");

   ---------------------------------------------------------------------------
   --  Core IRQ/Trap Dispatch
   ---------------------------------------------------------------------------
   procedure Handle_Interrupt
     (Frame_Ptr : in out Frame);

   procedure Handle_Trap
     (Frame_Ptr : access Frame)
     with Pre => Frame_Ptr /= null and then Frame_Ptr.sepc mod 4 = 0;
     pragma Export (C, Handle_Trap, "Handle_Trap");

   ---------------------------------------------------------------------------
   --  Device-tree IRQ Convenience
   ---------------------------------------------------------------------------
   procedure Register_Device_IRQ
     (Node    : access DTB_Node_Access;
      Handler : IRQ_Handler)
     with Pre => Node /= null and Handler /= null;

   ---------------------------------------------------------------------------
   --  Floating-point Context Management
   ---------------------------------------------------------------------------
   procedure Save_FP_Context    (Frame_Ptr : in out Frame);
   procedure Restore_FP_Context (Frame_Ptr : in out Frame);

end Arch.Interrupts;