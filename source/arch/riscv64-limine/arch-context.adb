--  ----------------------------------------------------------------
--  arch-context.adb: Architecture-specific context switching.
--  (RISC-V 64-bit version)
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

with Interfaces;          use Interfaces;
with Interfaces.C;        use Interfaces.C;
with System;              use System;
with System.Storage_Elements;
with System.Machine_Code;
with Memory.Physical;     use Memory.Physical;

with Arch.CPU;            use Arch.CPU;
with Arch.Local;          use Arch.Local;
with Arch.Interrupts;     use Arch.Interrupts;
with Arch.Snippets;       use Arch.Snippets;

package body Arch.Context is
   --  CSR access for misa register
   function Read_MISA return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("csrr %0, misa",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_MISA;

   --  Convert between System.Address and Unsigned_64
   function Addr_To_U64 is new Ada.Unchecked_Conversion (
     Source => System.Address,
     Target => Unsigned_64);
   function U64_To_Addr is new Ada.Unchecked_Conversion (
     Source => Unsigned_64,
     Target => System.Address);

   --  MISA extension bits and SSTATUS mask
   MISA_Value       : constant Unsigned_64 := Read_MISA;
   F_Extension_Bit  : constant Unsigned_64 := 2#100000#;  --  F-ext (bit 5)
   D_Extension_Bit  : constant Unsigned_64 := 2#1000000#; --  D-ext (bit 6)
   SSTATUS_SPP      : constant Unsigned_64 := 2#100000000#; --  SPP (bit 8)

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
         else
            FP_Save_Routine := Save_FP_Context_F'Access;
            FP_Load_Routine := Load_FP_Context_F'Access;
         end if;
      end if;
   end Setup_FP_Routines;

   --  Initialize GP context for new thread
   procedure Init_GP_Context (
     Ctx        : out GP_Context;
     Stack      : System.Address;
     Start_Addr : System.Address) is
      Frame_Buf : aliased Frame := (others => 0);
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
         Bytes : size_t := size_t (
            FP_Context'Storage_Size / Character'Size);
         Addr  : System.Address := Alloc (Bytes);
      begin
         Set_Memory (Addr, Bytes, 0);
         FP.FP_Context_Ptr := Addr;
      end;

      Ctx := FP.all;
      Setup_FP_Routines;
   end Init_GP_Context;

   --  Load saved GP context and return (no return)
   procedure Load_GP_Context (Ctx : GP_Context) with No_Return is
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
            System.Address'Asm_Input ("r", U64_To_Addr (FP.x2_sp))
         ),
         Clobber  => "memory",
         Volatile => True
      );
      loop
         null;
      end loop;
   end Load_GP_Context;

   --  Save current thread's core context ID
   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := Get_Current_Context;
   end Save_Core_Context;

   --  After fork: child returns zero & skips syscall
   procedure Success_Fork_Result (Ctx : in out GP_Context) is
      Frame_Buf : aliased Frame := Ctx;
      FP        : access Frame := Frame_Buf'Access;
   begin
      FP.x10_a0 := 0;
      FP.sepc   := FP.sepc + 4;
      Ctx       := FP.all;
   end Success_Fork_Result;

   --  Init FP context for new thread
   procedure Init_FP_Context (Ctx : out FP_Context) is
      Bytes : size_t := size_t (
         FP_Context'Storage_Size / Character'Size);
      Addr  : System.Address := Alloc (Bytes);
   begin
      Set_Memory (Addr, Bytes, 0);
      Ctx := Addr;
      Setup_FP_Routines;
      FP_Save_Routine.all (Ctx);
   end Init_FP_Context;

   --  Save FP context
   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      FP_Save_Routine.all (Ctx);
   end Save_FP_Context;

   --  Load FP context
   procedure Load_FP_Context (Ctx : FP_Context) is
   begin
      FP_Load_Routine.all (Ctx);
   end Load_FP_Context;

   --  Destroy FP context
   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
   begin
      Free (Ctx);
      Ctx := System.Null_Address;
   end Destroy_FP_Context;

   --  No-op FP save/load
   procedure FP_Save_NoOp (Ctx : in out FP_Context) is
   begin
      null;
   end FP_Save_NoOp;
   procedure FP_Load_NoOp (Ctx : FP_Context) is
   begin
      null;
   end FP_Load_NoOp;

   --  Single-precision FP save/restore
   procedure Save_FP_Context_F (Ctx : in out FP_Context) is
      Ptr : System.Address := Ctx;
   begin
      for Reg in 0 .. 31 loop
         Machine_Code.Asm (
           "fsw f" & Reg'Image & ", " &
           Integer'Image (Reg * 4) & "(%0)",
           Inputs   => System.Address'Asm_Input ("r", Ptr),
           Volatile => True
         );
      end loop;
   end Save_FP_Context_F;

   procedure Load_FP_Context_F (Ctx : FP_Context) is
      Ptr : System.Address := Ctx;
   begin
      for Reg in 0 .. 31 loop
         Machine_Code.Asm (
           "flw f" & Reg'Image & ", " &
           Integer'Image (Reg * 4) & "(%0)",
           Inputs   => System.Address'Asm_Input ("r", Ptr),
           Volatile => True
         );
      end loop;
   end Load_FP_Context_F;

   --  Double-precision FP save/restore
   procedure Save_FP_Context_D (Ctx : in out FP_Context) is
      Ptr : System.Address := Ctx;
   begin
      for Reg in 0 .. 31 loop
         Machine_Code.Asm (
           "fsd f" & Reg'Image & ", " &
           Integer'Image (Reg * 8) & "(%0)",
           Inputs   => System.Address'Asm_Input ("r", Ptr),
           Volatile => True
         );
      end loop;
   end Save_FP_Context_D;

   procedure Load_FP_Context_D (Ctx : FP_Context) is
      Ptr : System.Address := Ctx;
   begin
      for Reg in 0 .. 31 loop
         Machine_Code.Asm (
           "fld f" & Reg'Image & ", " &
           Integer'Image (Reg * 8) & "(%0)",
           Inputs   => System.Address'Asm_Input ("r", Ptr),
           Volatile => True
         );
      end loop;
   end Load_FP_Context_D;

end Arch.Context;
