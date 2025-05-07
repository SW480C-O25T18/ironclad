--  arch-context.adb: CPU management routines.
--  Handles CPU initialization, core management, and exception handling.
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

pragma SPARK_Mode (Off);
pragma Warnings (Off, "SPARK_Mode is disabled");

with Interfaces.C;        use Interfaces.C;
with System;              use System;
with System.Machine_Code; use System.Machine_Code;
with Memory.Physical;     use Memory.Physical;
with Ada.Unchecked_Conversion;
with Arch.Local;          use Arch.Local;
with Arch.Debug;          use Arch.Debug;
with Lib.Panic;           use Lib.Panic;
with Arch.Interrupts;     use Arch.Interrupts;

package body Arch.Context is

   type Unsigned_8_Array is array (Positive range <>) of Unsigned_8;

   ----------------------------------------------------------------------------
   --  Global Variables for Floating-Point Context Routines
   ----------------------------------------------------------------------------
   FP_Init    : access procedure (Ctx : out FP_Context);
   FP_Save    : access procedure (Ctx : in out FP_Context);
   FP_Load    : access procedure (Ctx : FP_Context);
   FP_Destroy : access procedure (Ctx : in out FP_Context);

   ----------------------------------------------------------------------------
   --  Helper Functions for Type Conversions
   ----------------------------------------------------------------------------
   function Addr_To_U64 is new Ada.Unchecked_Conversion
     (Source => System.Address, 
      Target => Interfaces.Unsigned_64);

   function FP_Context_To_Addr is new
      Ada.Unchecked_Conversion (
         Source => FP_Context,
         Target => System.Address);

   function Addr_To_FP_Context is new
      Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => FP_Context);

   ----------------------------------------------------------------------------
   --  Set_Memory: Initialize a block of memory to a specified value
   ----------------------------------------------------------------------------
   procedure Set_Memory
     (Addr : System.Address;
      Size : System.Storage_Elements.Storage_Count;
      Value : Unsigned_8) is
      Ptr : Unsigned_8_Array (1 .. Size)
        with Address => Addr;
   begin
      if Size > 0 then
         for I in 1 .. Size loop
            Ptr (I) := Value;
         end loop;
      else
         Arch.Debug.Print("Set_Memory: Size is zero, no memory initialized");
      end if;
   exception
      when others =>
         Arch.Debug.Print("Set_Memory: Exception encountered");
         Lib.Panic.Hard_Panic("Set_Memory: Failed to initialize memory");
   end Set_Memory;

   ----------------------------------------------------------------------------
   --  Floating-Point Context Routines for D Extension
   ----------------------------------------------------------------------------
   procedure Init_FP_Context_D (Ctx : out FP_Context) is
      Addr : System.Address :=
        System.Address(Memory.Physical.Alloc(FP_Context'Size));
   begin
      Set_Memory(Addr, System.Storage_Elements.Storage_Count(FP_Context'Size), 0);
      Ctx := Addr_To_FP_Context(Addr);
      Arch.Debug.Print("Init_FP_Context_D: Initialized D extension context");
   exception
      when others =>
         Arch.Debug.Print("Init_FP_Context_D: Exception encountered");
         Lib.Panic.Hard_Panic("Init_FP_Context_D: Exception");
   end Init_FP_Context_D;

   procedure Save_FP_Context_D (Ctx : in out FP_Context) is
   begin
      Machine_Code.Asm(
         "fsd f0, 0(%0); fsd f1, 8(%0); fsd f2, 16(%0); fsd f3, 24(%0);" &
         "fsd f4, 32(%0); fsd f5, 40(%0); fsd f6, 48(%0); fsd f7, 56(%0);" &
         "fsd f8, 64(%0); fsd f9, 72(%0); fsd f10, 80(%0); fsd f11, 88(%0);" &
         "fsd f12, 96(%0); fsd f13, 104(%0); fsd f14, 112(%0); fsd f15, 120(%0);" &
         "fsd f16, 128(%0); fsd f17, 136(%0); fsd f18, 144(%0); fsd f19, 152(%0);" &
         "fsd f20, 160(%0); fsd f21, 168(%0); fsd f22, 176(%0); fsd f23, 184(%0);" &
         "fsd f24, 192(%0); fsd f25, 200(%0); fsd f26, 208(%0); fsd f27, 216(%0);" &
         "fsd f28, 224(%0); fsd f29, 232(%0); fsd f30, 240(%0); fsd f31, 248(%0)",
         Inputs => (System.Address'Asm_Input("r", FP_Context_To_Addr(Ctx))),
         Clobber => "memory",
         Volatile => True
      );
   exception
      when others =>
         Arch.Debug.Print("Save_FP_Context_D: Exception encountered");
         Lib.Panic.Hard_Panic("Save_FP_Context_D: Exception");
   end Save_FP_Context_D;

   procedure Load_FP_Context_D (Ctx : FP_Context) is
   begin
      Machine_Code.Asm(
         "fld f0, 0(%0); fld f1, 8(%0); fld f2, 16(%0); fld f3, 24(%0);" &
         "fld f4, 32(%0); fld f5, 40(%0); fld f6, 48(%0); fld f7, 56(%0);" &
         "fld f8, 64(%0); fld f9, 72(%0); fld f10, 80(%0); fld f11, 88(%0);" &
         "fld f12, 96(%0); fld f13, 104(%0); fld f14, 112(%0); fld f15, 120(%0);" &
         "fld f16, 128(%0); fld f17, 136(%0); fld f18, 144(%0); fld f19, 152(%0);" &
         "fld f20, 160(%0); fld f21, 168(%0); fld f22, 176(%0); fld f23, 184(%0);" &
         "fld f24, 192(%0); fld f25, 200(%0); fld f26, 208(%0); fld f27, 216(%0);" &
         "fld f28, 224(%0); fld f29, 232(%0); fld f30, 240(%0); fld f31, 248(%0)",
         Inputs => (System.Address'Asm_Input("r", FP_Context_To_Addr(Ctx))),
         Clobber => "memory",
         Volatile => True
      );
   exception
      when others =>
         Arch.Debug.Print("Load_FP_Context_D: Exception encountered");
         Lib.Panic.Hard_Panic("Load_FP_Context_D: Exception");
   end Load_FP_Context_D;

   procedure Destroy_FP_Context_D (Ctx : in out FP_Context) is
   begin
      Memory.Physical.Free(
         System.Storage_Elements.Storage_Count(Addr_To_U64(FP_Context_To_Addr(Ctx))));
      Ctx := Addr_To_FP_Context(System.Null_Address);
   exception
      when others =>
         Arch.Debug.Print("Destroy_FP_Context_D: Exception encountered");
         Ctx := Addr_To_FP_Context(System.Null_Address);
   end Destroy_FP_Context_D;

   ----------------------------------------------------------------------------
   --  General-Purpose Context Management
   ----------------------------------------------------------------------------

   --  Initialize GP context for a new thread
   procedure Init_GP_Context (
      Ctx        : out GP_Context;
      Stack      : System.Address;
      Start_Addr : System.Address
   ) is
   begin
      -- Initialize the general-purpose context
      Ctx.sepc := Addr_To_U64(Start_Addr);
      Ctx.x2_sp := Addr_To_U64(Stack);
      Ctx.x4_tp := Addr_To_U64(Arch.Local.Fetch_TCB);
      Ctx.scause := 0;
      Ctx.stval := 0;
      Ctx.sstatus := 0; -- Default supervisor status
   exception
      when others =>
         Arch.Debug.Print("Init_GP_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Init_GP_Context: Exception");
   end Init_GP_Context;

   --  Load saved GP context and return (no return)
   procedure Load_GP_Context (Ctx : GP_Context) is
   begin
      Machine_Code.Asm(
         "csrw sepc, %0; csrw sstatus, %1; mv sp, %2; sret",
         Inputs => (
            Unsigned_64'Asm_Input("r", Ctx.sepc),
            Unsigned_64'Asm_Input("r", Ctx.sstatus),
            Unsigned_64'Asm_Input("r", Ctx.x2_sp)
         ),
         Clobber  => "memory",
         Volatile => True
      );
      loop
         null;
      end loop;
   exception
      when others =>
         Arch.Debug.Print("Load_GP_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Load_GP_Context: Exception");
   end Load_GP_Context;

begin
   --  Detect supported floating-point extensions
   Detect_FP_Extensions;

end Arch.Context;