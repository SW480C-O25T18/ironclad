pragma SPARK_Mode (Off);
pragma Warnings (Off, "SPARK_Mode is disabled");

with Interfaces.C;        use Interfaces.C;
with System;              use System;
with System.Machine_Code; use System.Machine_Code;
with Memory.Physical;     use Memory.Physical;
with Ada.Unchecked_Conversion;
with Arch.CPU;            use Arch.CPU;
with Arch.Local;          use Arch.Local;
with Arch.Interrupts;     use Arch.Interrupts;
with Arch.Debug;          use Arch.Debug;
with Arch.Snippets;       use Arch.Snippets;
with Lib.Panic;           use Lib.Panic;

package body Arch.Context is

   ----------------------------------------------------------------------------
   --  Helper Functions for Type Conversions
   ----------------------------------------------------------------------------

   --  Convert a FP_Context to a System.Address
   function FP_Context_To_Addr is new
      Ada.Unchecked_Conversion (
         Source => FP_Context,
         Target => System.Address);

   --  Convert a System.Address to a FP_Context
   function Addr_To_FP_Context is new
      Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => FP_Context);

   --  Convert a System.Address to an unsigned 64-bit integer
   function Addr_To_U64 is new
      Ada.Unchecked_Conversion (
         Source => System.Address,
         Target => Unsigned_64);

   --  Convert a 64-bit integer back to an Address
   function U64_To_Addr is new
      Ada.Unchecked_Conversion (
         Source => Unsigned_64,
         Target => System.Address);

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
      Ctx.sstatus := Arch.CPU.Read_SStatus and not Arch.CPU.SSTATUS_SPP;
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

   --  Save architectural task data that does not fit within GP or FP data
   procedure Save_Core_Context (Ctx : out Core_Context) is
   begin
      Ctx := Arch.Local.Get_Current_Context;
   exception
      when others =>
         Arch.Debug.Print("Save_Core_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Save_Core_Context: Exception");
   end Save_Core_Context;

   --  After fork: child returns zero & skips syscall
   procedure Success_Fork_Result (Ctx : in out GP_Context) is
   begin
      Ctx.x10_a0 := 0; -- Set return value to 0 for child process
   exception
      when others =>
         Arch.Debug.Print("Success_Fork_Result: Exception encountered");
         Lib.Panic.Hard_Panic("Success_Fork_Result: Exception");
   end Success_Fork_Result;

   ----------------------------------------------------------------------------
   --  Floating-Point Context Management
   ----------------------------------------------------------------------------

   --  Initialize FP context for a new thread
   procedure Init_FP_Context (Ctx : out FP_Context) is
      Addr : System.Address := System.Address(Memory.Physical.Alloc(FP_Context'Size));
   begin
      -- Ensure memory is zeroed out
      Set_Memory(Addr, System.Storage_Elements.Integer_Address(FP_Context'Size), 0);
      Ctx := Addr_To_FP_Context(Addr);
   exception
      when others =>
         Arch.Debug.Print("Init_FP_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Init_FP_Context: Exception");
   end Init_FP_Context;

   --  Save FP context
   procedure Save_FP_Context (Ctx : in out FP_Context) is
   begin
      Arch.CPU.Save_FP_State(Ctx);
   exception
      when others =>
         Arch.Debug.Print("Save_FP_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Save_FP_Context: Exception");
   end Save_FP_Context;

   --  Load FP context
   procedure Load_FP_Context (Ctx : FP_Context) is
   begin
      Arch.CPU.Load_FP_State(Ctx);
   exception
      when others =>
         Arch.Debug.Print("Load_FP_Context: Exception encountered");
         Lib.Panic.Hard_Panic("Load_FP_Context: Exception");
   end Load_FP_Context;

   --  Destroy FP context
   procedure Destroy_FP_Context (Ctx : in out FP_Context) is
   begin
      -- Free allocated memory
      Memory.Physical.Free(Addr_To_U64(FP_Context_To_Addr(Ctx)));
      Ctx := Addr_To_FP_Context(System.Null_Address);
   exception
      when others =>
         Arch.Debug.Print("Destroy_FP_Context: Exception encountered");
         Ctx := Addr_To_FP_Context(System.Null_Address); -- Ensure Ctx is nullified
   end Destroy_FP_Context;

end Arch.Context;