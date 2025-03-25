--  arch-exceptions.ads: Specification of Platform-Level Interrupt Controller (PLIC) utilities.
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
--  along with this program.  If not, see <http://www.gnu.

with System;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Assertions;         -- For pragma Assert
with System.Machine_Code;    -- For inline assembly
with Arch.Debug;             -- For Arch.Debug.Print messages        

package body Arch.PLIC is
   ------------------------------------------------------------------------------
   --  Protected Configuration Object for PLIC Settings
   --  All dynamic configuration values come from the DTB (or configuration module).
   --  The Enabled flag indicates whether the PLIC is supported on this platform.
   ------------------------------------------------------------------------------
   protected PLIC_Config is
      procedure Set (
         Base_Address         : System.Address;
         Priority_Offset      : Unsigned_64;
         Context_Base_Offset  : Unsigned_64;
         Context_Stride       : Unsigned_64;
         Max_Interrupt_ID     : Unsigned_64;
         Max_Harts            : Unsigned_64;
         Contexts_Per_Hart    : Unsigned_64;
         Enabled              : Boolean
      );
      function Get_Base return System.Address;
      function Get_Priority_Offset return Unsigned_64;
      function Get_Context_Base return Unsigned_64;
      function Get_Context_Stride return Unsigned_64;
      function Get_Max_Interrupt_ID return Unsigned_64;
      function Get_Max_Harts return Unsigned_64;
      function Get_Contexts_Per_Hart return Unsigned_64;
      function Get_Enabled return Boolean;
   private
      Base : System.Address := System'To_Address(16#0C000000#);
      Priority_Offset : Unsigned_64 := 0;
      Context_Base : Unsigned_64 := 16#200000#;
      Context_Stride : Unsigned_64 := 16#1000#;
      Max_Interrupt_ID : Unsigned_64 := 1023;
      Max_Harts : Unsigned_64 := 1;
      Contexts_Per_Hart : Unsigned_64 := 1;
      Enabled : Boolean := True;
   end PLIC_Config;

   protected body PLIC_Config is
      procedure Set (
         Base_Address         : System.Address;
         Priority_Offset      : Unsigned_64;
         Context_Base_Offset  : Unsigned_64;
         Context_Stride       : Unsigned_64;
         Max_Interrupt_ID     : Unsigned_64;
         Max_Harts            : Unsigned_64;
         Contexts_Per_Hart    : Unsigned_64;
         Enabled              : Boolean
      ) is
      begin
         Base := Base_Address;
         Priority_Offset := Priority_Offset;
         Context_Base := Context_Base_Offset;
         Context_Stride := Context_Stride;
         Max_Interrupt_ID := Max_Interrupt_ID;
         Max_Harts := Max_Harts;
         Contexts_Per_Hart := Contexts_Per_Hart;
         Self.Enabled := Enabled;
      end Set;

      function Get_Base return System.Address is
      begin
         return Base;
      end Get_Base;

      function Get_Priority_Offset return Unsigned_64 is
      begin
         return Priority_Offset;
      end Get_Priority_Offset;

      function Get_Context_Base return Unsigned_64 is
      begin
         return Context_Base;
      end Get_Context_Base;

      function Get_Context_Stride return Unsigned_64 is
      begin
         return Context_Stride;
      end Get_Context_Stride;

      function Get_Max_Interrupt_ID return Unsigned_64 is
      begin
         return Max_Interrupt_ID;
      end Get_Max_Interrupt_ID;

      function Get_Max_Harts return Unsigned_64 is
      begin
         return Max_Harts;
      end Get_Max_Harts;

      function Get_Contexts_Per_Hart return Unsigned_64 is
      begin
         return Contexts_Per_Hart;
      end Get_Contexts_Per_Hart;

      function Get_Enabled return Boolean is
      begin
         return Enabled;
      end Get_Enabled;
   end PLIC_Config;

   ------------------------------------------------------------------------------
   --  DTB Configuration Setters/Getters
   ------------------------------------------------------------------------------
   procedure Set_PLIC_Configuration (
      Base_Address         : System.Address;
      Priority_Offset      : Unsigned_64;
      Context_Base_Offset  : Unsigned_64;
      Context_Stride       : Unsigned_64;
      Max_Interrupt_ID     : Unsigned_64;
      Max_Harts            : Unsigned_64;
      Contexts_Per_Hart    : Unsigned_64;
      Enabled              : Boolean := True
   ) is
   begin
      PLIC_Config.Set(Base_Address, Priority_Offset,
                      Context_Base_Offset, Context_Stride,
                      Max_Interrupt_ID, Max_Harts, Contexts_Per_Hart,
                      Enabled);
   end Set_PLIC_Configuration;

   function Get_PLIC_Base return System.Address is
   begin
      Arch.Debug.Print("Get_PLIC_Base: " & Unsigned_64'Image(PLIC_Config.Get_Base));
      return PLIC_Config.Get_Base;
   end Get_PLIC_Base;

   function Get_Priority_Offset return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Priority_Offset: " & Unsigned_64'Image(PLIC_Config.Get_Priority_Offset));
      return PLIC_Config.Get_Priority_Offset;
   end Get_Priority_Offset;

   function Get_Context_Base return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Context_Base: " & Unsigned_64'Image(PLIC_Config.Get_Context_Base));
      return PLIC_Config.Get_Context_Base;
   end Get_Context_Base;

   function Get_Context_Stride return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Context_Stride: " & Unsigned_64'Image(PLIC_Config.Get_Context_Stride));
      return PLIC_Config.Get_Context_Stride;
   end Get_Context_Stride;

   function Get_Max_Interrupt_ID return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Max_Interrupt_ID: " & Unsigned_64'Image(PLIC_Config.Get_Max_Interrupt_ID));
      return PLIC_Config.Get_Max_Interrupt_ID;
   end Get_Max_Interrupt_ID;

   function Get_Max_Harts return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Max_Harts: " & Unsigned_64'Image(PLIC_Config.Get_Max_Harts));
      return PLIC_Config.Get_Max_Harts;
   end Get_Max_Harts;

   function Get_Contexts_Per_Hart return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Contexts_Per_Hart: " & Unsigned_64'Image(PLIC_Config.Get_Contexts_Per_Hart));
      return PLIC_Config.Get_Contexts_Per_Hart;
   end Get_Contexts_Per_Hart;

   function Is_Enabled return Boolean is
   begin
      Arch.Debug.Print("Is_Enabled: " & Boolean'Image(PLIC_Config.Get_Enabled));
      return PLIC_Config.Get_Enabled;
   end Is_Enabled;

   ------------------------------------------------------------------------------
   --  Helper Function: PLIC_Address
   --  Computes an absolute address by adding an offset to the configured PLIC base.
   ------------------------------------------------------------------------------
   function PLIC_Address (Offset : Unsigned_64) return System.Address is
      use System.Storage_Elements;
      Base_Int : constant Integer := To_Integer(To_Address(Get_PLIC_Base));
   begin
      Arch.Debug.Print("PLIC_Address: Base: " & Unsigned_64'Image(To_Integer(Get_PLIC_Base)));
      return To_Address(Base_Int + To_Integer(Offset));
   end PLIC_Address;

   ------------------------------------------------------------------------------
   --  Helper Function: Context_Offset
   --  Computes the offset for a given Hart and Context using dynamic configuration.
   ------------------------------------------------------------------------------
   function Context_Offset (Hart_ID : Unsigned_64; Context_ID : Unsigned_64) return Unsigned_64 is
   begin
      return Get_Context_Base + (Hart_ID * Get_Context_Stride) + (Context_ID * Get_Context_Stride);
   end Context_Offset;

   ------------------------------------------------------------------------------
   --  Volatile Register Pointer Type
   --  Used for memory-mapped register accesses.
   ------------------------------------------------------------------------------
   type Reg_Type is new Unsigned_64;
   pragma Volatile(Reg_Type);
   -- Access type for volatile register pointers
   type Reg_Ptr is access all Reg_Type;
   function Reg (Abs_Addr : System.Address) return Reg_Ptr is
   begin
      Arch.Debug.Print("Reg: Address: " & Unsigned_64'Image(To_Integer(Abs_Addr)));
      return Reg_Ptr(Abs_Addr);
      Arch.Debug.Print("Reg: Address End");
   end Reg;

   ------------------------------------------------------------------------------
   --  Memory_Barrier
   --  Issues a RISC-V fence instruction to enforce ordering of memory operations.
   ------------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      Arch.Debug.Print("Memory barrier Start");
      System.Machine_Code.Atomic_Load_Store(
         Atomic_Operation => System.Machine_Code.Fence,
         Address          => System.Null_Address,
         Value            => 0,
         Size             => 0
      );
      Arch.Debug.Print("Memory barrier End");
   end Memory_Barrier;

   ------------------------------------------------------------------------------
   -- Check if PLIC is enabled. If not, print a debug message and return False.
   ------------------------------------------------------------------------------
   procedure Check_PLIC_Supported return Boolean is
   begin
      if not Is_Enabled then
         Arch.Debug.Print("PLIC not supported on this platform.");
         return False;
      else
         Arch.Debug.Print("PLIC is supported on this platform.");
         return True;
      end if;
   end Check_PLIC_Supported;

   ------------------------------------------------------------------------------
   --  Initialize
   --  Initializes the PLIC for a given Hart and Context.
   ------------------------------------------------------------------------------
   procedure Initialize (Hart_ID   : Unsigned_64;
                         Context_ID: Unsigned_64 := 0) is
      Ctx_Base      : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : Reg_Ptr;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Initialize: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Initialize: Start");
      Threshold_Reg := Reg(PLIC_Address(Ctx_Base));
      Arch.Debug.Print("Initialize: Threshold register address: " & Unsigned_64'Image(Threshold_Reg'Address));
      pragma Assert (Ctx_Base < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
                       "Threshold register address out of bounds");
      -- Initialize the threshold register to 0 (all interrupts enabled)
      Arch.Debug.Print("Initialize: Writing 0 to threshold register");
      Threshold_Reg.all := 0;
      Arch.Debug.Print("Initialize: Writing 0 End");
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;
      Arch.Debug.Print("Initialize: End");
   end Initialize;

   ------------------------------------------------------------------------------
   --  Claim
   --  Claims the highest-priority pending interrupt for a given Hart and Context.
   ------------------------------------------------------------------------------
   function Claim (Hart_ID   : Unsigned_64;
                   Context_ID: Unsigned_64 := 0) return Unsigned_64 is
      Ctx_Base : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Claim_Reg : Reg_Ptr;
      Claimed_ID : Unsigned_64;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Claim: PLIC is disabled.");
         return 0;
      end if;
      Arch.Debug.Print("Claim: Start");
      Claim_Reg := Reg(PLIC_Address(Ctx_Base + 4));
      Arch.Debug.Print("Claim: Claim register address: " & Unsigned_64'Image(Claim_Reg'Address));
      pragma Assert (Ctx_Base + 4 < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
               "Claim register address out of bounds");
      -- Claim the highest-priority pending interrupt
      Arch.Debug.Print("Claim: Reading from claim register");
      Claimed_ID := Claim_Reg.all;
      Arch.Debug.Print("Claim: Claimed ID: " & Unsigned_64'Image(Claimed_ID));
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;
      Arch.Debug.Print("Claim: End");
      return Claimed_ID;
   end Claim;

   ------------------------------------------------------------------------------
   --  Complete
   --  Completes the interrupt handling for a given Hart and Context.
   ------------------------------------------------------------------------------
   procedure Complete (Hart_ID   : Unsigned_64;
                       Context_ID: Unsigned_64 := 0;
                       Interrupt_ID : Unsigned_64) is
      Ctx_Base : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Claim_Reg : Reg_Ptr;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Complete: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Complete: Start");
      Claim_Reg := Reg(PLIC_Address(Ctx_Base + 4));
      Arch.Debug.Print("Complete: Claim register address: " & Unsigned_64'Image(Claim_Reg'Address));
      pragma Assert (Ctx_Base + 4 < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
               "Claim register address out of bounds");
      -- Complete the interrupt handling by writing the claimed ID back to the register
      Arch.Debug.Print("Complete: Writing claimed ID back to claim register");
      Claim_Reg.all := Interrupt_ID;
      Arch.Debug.Print("Complete: Writing claimed ID End");
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;
      Arch.Debug.Print("Complete: End");
   end Complete;

   ------------------------------------------------------------------------------
end Arch.PLIC;