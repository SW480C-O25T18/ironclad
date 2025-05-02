--  ----------------------------------------------------------------
--  arch-context.adb: Architecture-specific context switching.
--  (RISC-V 64-bit version)
--  Optimized for time and space efficiency with proper type handling.
--  Copyright (C) 2025 scweeks
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
--  ----------------------------------------------------------------

pragma SPARK_Mode (Off);
pragma Warnings (Off, "SPARK_Mode is disabled");

with Interfaces.C;        use Interfaces.C;
with System;              use System;
with System.Machine_Code; use System.Machine_Code;
with Memory.Physical;     use Memory.Physical;
with Ada.Unchecked_Conversion;
with Arch.CPU;            use Arch.CPU;
with Arch.Local;          use Arch.Local;
with Arch.Interrupts;      use Arch.Interrupts;
with Arch.Debug;         use Arch.Debug;
with Arch.Snippets;       use Arch.Snippets;
with Lib.Panic;         use Lib.Panic;

package body Arch.Context is

   type Byte_Ptr is access all Unsigned_8;
   ----------------------------------------------------------------------------
   --  Helper Functions for Type Conversions
   ----------------------------------------------------------------------------

   --  Convert a System.Address to an unsigned 64-bit integer
   function Addr_To_U64 (A : System.Address) return Unsigned_64 is
   begin
      return Unsigned_64 (A);
   end Addr_To_U64;

   --  Convert a 64-bit integer back to an Address
   function U64_To_Addr (V : Unsigned_64) return System.Address is
   begin
      return System.Address (V);
   end U64_To_Addr;

   --  Convert a System.Address to a Byte_Ptr
   function To_Byte_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Ptr);

   ----------------------------------------------------------------------------
   --  Memory Utilities
   ----------------------------------------------------------------------------

   --  Write “Len” bytes of value “B” starting at address “Base”
   procedure Set_Memory (
      Base : System.Address;
      Len  : System.Storage_Elements.Integer_Address;
      B    : Unsigned_8
   ) is
      P : Byte_Ptr := To_Byte_Ptr (Base);
   begin
      if P /= null and then Len > 0 then
         for I in 1 .. Integer (Len) loop
            P.all := B;
            P := P + 1;
         end loop;
      end if;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Set_Memory: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Set_Memory: Constraint_Error");
   end Set_Memory;

   --  “Free” is a no-op for context buffers
   procedure Free_Memory (
      Base : System.Address;
      Len  : System.Storage_Elements.Integer_Address
   ) is
   begin
      null;
   end Free_Memory;

   ----------------------------------------------------------------------------
   --  Floating-Point Context Management
   ----------------------------------------------------------------------------

   --  No-op FP save/load
   procedure FP_Save_NoOp (Ctx : in out FP_Context) is
   begin
      null;
   end FP_Save_NoOp;

   procedure FP_Load_NoOp (Ctx : FP_Context) is
   begin
      null;
   end FP_Load_NoOp;

   --  CSR access for misa register
   function Read_MISA return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Machine_Code.Asm ("csrr %0, misa",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_MISA;

   --  MISA extension bits and SSTATUS mask
   MISA_Value       : constant Unsigned_64 := Read_MISA;
   F_Extension_Bit  : constant Unsigned_64 := 2#100000#;  --  F-ext (bit 5)
   D_Extension_Bit  : constant Unsigned_64 := 2#1000000#; --  D-ext (bit 6)
   SSTATUS_SPP      : constant Unsigned_64 := 2#100000000#; --  SPP (bit 8)

   ----------------------------------------------------------------------------
   --  Floating-Point Context Management
   ----------------------------------------------------------------------------

   -- Save FP context for double-precision (D-extension)
   procedure Save_FP_Context_D (Ctx : in out FP_Context) is
   begin
      Arch.Debug.Print("Save_FP_Context_D: Saving double-precision FP context");
      Machine_Code.Asm("fsd f0, 0(%0); fsd f1, 8(%0); fsd f2, 16(%0); ...",
         Inputs => (Unsigned_64'Asm_Input("r", Addr_To_U64(Ctx))),
         Clobber => "memory",
         Volatile => True);
   end Save_FP_Context_D;

   -- Load FP context for double-precision (D-extension)
   procedure Load_FP_Context_D (Ctx : FP_Context) is
   begin
      Arch.Debug.Print("Load_FP_Context_D: Loading double-precision FP context");
      Machine_Code.Asm("fld f0, 0(%0); fld f1, 8(%0); fld f2, 16(%0); ...",
         Inputs => (Unsigned_64'Asm_Input("r", Addr_To_U64(Ctx))),
         Clobber => "memory",
         Volatile => True);
   end Load_FP_Context_D;

   -- Save FP context for single-precision (F-extension)
   procedure Save_FP_Context_F (Ctx : in out FP_Context) is
   begin
      Arch.Debug.Print("Save_FP_Context_F: Saving single-precision FP context");
      Machine_Code.Asm("fsw f0, 0(%0); fsw f1, 4(%0); fsw f2, 8(%0); ...",
         Inputs => (Unsigned_64'Asm_Input("r", Addr_To_U64(Ctx))),
         Clobber => "memory",
         Volatile => True);
   end Save_FP_Context_F;

   -- Load FP context for single-precision (F-extension)
   procedure Load_FP_Context_F (Ctx : FP_Context) is
   begin
      Arch.Debug.Print("Load_FP_Context_F: Loading single-precision FP context");
      Machine_Code.Asm("flw f0, 0(%0); flw f1, 4(%0); flw f2, 8(%0); ...",
         Inputs => (Unsigned_64'Asm_Input("r", Addr_To_U64(Ctx))),
         Clobber => "memory",
         Volatile => True);
   end Load_FP_Context_F;
   
   --  FP save/load dispatch types
   type FP_Save_Routine_Type is access procedure (Ctx : in out FP_Context);
   type FP_Load_Routine_Type is access procedure (Ctx : FP_Context);

   FP_Save_Routine : FP_Save_Routine_Type := FP_Save_NoOp'Access;
   FP_Load_Routine : FP_Load_Routine_Type := FP_Load_NoOp'Access;

   --  Select FP routines based on MISA
   procedure Setup_FP_Routines is
   begin
      if (MISA_Value and F_Extension_Bit) /= 0 then
         if (MISA_Value and D_Extension_Bit) /= 0 then
            FP_Save_Routine := Save_FP_Context_D'Access;
            FP_Load_Routine := Load_FP_Context_D'Access;
            Arch.Debug.Print("Setup_FP_Routines: Using double-precision FP routines");
         else
            FP_Save_Routine := Save_FP_Context_F'Access;
            FP_Load_Routine := Load_FP_Context_F'Access;
            Arch.Debug.Print("Setup_FP_Routines: Using single-precision FP routines");
         end if;
      else
         FP_Save_Routine := FP_Save_NoOp'Access;
         FP_Load_Routine := FP_Load_NoOp'Access;
         Arch.Debug.Print("Setup_FP_Routines: No FP extensions detected, using no-op routines");
      end if;
   end Setup_FP_Routines;

   -- Private helper function in arch-context.adb
   function Get_Current_Context return Core_Context is
      TCB : constant System.Address := Arch.Local.Fetch_TCB;
   begin
      if TCB = System.Null_Address then
         Arch.Debug.Print ("Get_Current_Context: TCB is null");
         return 0;
      else
         return Core_Context (
            Address_To_Unsigned_64 (TCB)
            + Interrupts.Get_TCP_OFFSET);
      end if;
   end Get_Current_Context;

   ----------------------------------------------------------------------------
   --  General-Purpose Context Management
   ----------------------------------------------------------------------------

   --  Initialize GP context for new thread
   procedure Init_GP_Context (
      Ctx        : out GP_Context;
      Stack      : System.Address;
      Start_Addr : System.Address
   ) is
      Frame_Buf : aliased Frame;
      FP        : access Frame := Frame_Buf'Access;
   begin
      pragma Assert (Stack /= System.Null_Address);
      pragma Assert (Start_Addr /= System.Null_Address);

      FP.sepc     := Addr_To_U64 (Start_Addr);
      FP.x2_sp    := Addr_To_U64 (Stack);
      FP.x4_tp    := Addr_To_U64 (Fetch_TCB);
      FP.scause   := 0;
      FP.stval    := 0;
      FP.sstatus  := Read_SStatus and not SSTATUS_SPP;

      declare
         Bytes : size_t := size_t (FP_Context'Size / Character'Size);
         Addr  : System.Address := System.Address (Alloc (Bytes));
      begin
         Set_Memory (Addr, System.Storage_Elements.Integer_Address (Bytes), 0);
         FP.FP_Context_Ptr := Addr;
      end;

      Ctx := FP.all;
      Setup_FP_Routines;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Init_GP_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Init_GP_Context: Constraint_Error");
   end Init_GP_Context;

   --  Load saved GP context and return (no return)
   procedure Load_GP_Context (Ctx : GP_Context) is
      Frame_Buf : aliased Frame := Ctx;
      FP        : access Frame := Frame_Buf'Access;
   begin
      pragma Assert (FP.x2_sp /= 0);
      pragma Assert (FP.sepc   /= 0);

      Machine_Code.Asm (
         "csrw sepc, %0; csrw sstatus, %1; mv sp, %2; sret",
         Inputs => (
            Unsigned_64'Asm_Input ("r", FP.sepc),
            Unsigned_64'Asm_Input ("r", FP.sstatus),
            Unsigned_64'Asm_Input ("r", FP.x2_sp)
         ),
         Clobber  => "memory",
         Volatile => True
      );
      loop
         null;
      end loop;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Load_GP_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Load_GP_Context: Constraint_Error");
   end Load_GP_Context;

   --  Save current thread's core context ID
   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := Arch.Local.Get_Current_Context;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Save_Core_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Save_Core_Context: Constraint_Error");
   end Save_Core_Context;

   --  After fork: child returns zero & skips syscall
   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx.x10_a0 := 0; -- Set return value to 0 for child process
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Success_Fork_Result: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Success_Fork_Result: Constraint_Error");
   end Success_Fork_Result;

   ----------------------------------------------------------------------------
   --  Floating-Point Context Management
   ----------------------------------------------------------------------------

   --  Init FP context for new thread
   procedure Init_FP_Context (Ctx : out FP_Context) is
      Addr  : System.Address := FP_Context'Address;
   begin
      Set_Memory (Addr,
         System.Storage_Elements.Integer_Address (FP_Context'Size), 0);
      Ctx := Addr;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Init_FP_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Init_FP_Context: Constraint_Error");
   end Init_FP_Context;

   --  Save FP context
   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      FP_Save_Routine.all (Ctx);
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Save_FP_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Save_FP_Context: Constraint_Error");
   end Save_FP_Context;

   --  Load FP context
   procedure Load_FP_Context (Ctx : FP_Context) is
   begin
      FP_Load_Routine.all (Ctx);
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Load_FP_Context: Constraint_Error encountered");
         Lib.Panic.Hard_Panic ("Load_FP_Context: Constraint_Error");
   end Load_FP_Context;

   --  Destroy FP context
   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
   begin
      Free_Memory (
         U64_To_Addr (Unsigned_64 (Ctx)),
         System.Storage_Elements.Integer_Address (FP_Context'Size)
      );
      Ctx := System.Null_Address;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("Destroy_FP_Context: Constraint_Error encountered");
         Ctx := System.Null_Address; -- Ensure Ctx is nullified to avoid further issues
   end Destroy_FP_Context;

end Arch.Context;