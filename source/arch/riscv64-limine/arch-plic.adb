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
      return PLIC_Config.Get_Base;
   end Get_PLIC_Base;

   function Get_Priority_Offset return Unsigned_64 is
   begin
      return PLIC_Config.Get_Priority_Offset;
   end Get_Priority_Offset;

   function Get_Context_Base return Unsigned_64 is
   begin
      return PLIC_Config.Get_Context_Base;
   end Get_Context_Base;

   function Get_Context_Stride return Unsigned_64 is
   begin
      return PLIC_Config.Get_Context_Stride;
   end Get_Context_Stride;

   function Get_Max_Interrupt_ID return Unsigned_64 is
   begin
      return PLIC_Config.Get_Max_Interrupt_ID;
   end Get_Max_Interrupt_ID;

   function Get_Max_Harts return Unsigned_64 is
   begin
      return PLIC_Config.Get_Max_Harts;
   end Get_Max_Harts;

   function Get_Contexts_Per_Hart return Unsigned_64 is
   begin
      return PLIC_Config.Get_Contexts_Per_Hart;
   end Get_Contexts_Per_Hart;

   function Is_Enabled return Boolean is
   begin
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
      return Reg_Ptr(Abs_Addr);
   end Reg;

   ------------------------------------------------------------------------------
   --  Memory_Barrier
   --  Issues a RISC-V fence instruction to enforce ordering of memory operations.
   ------------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      System.Machine_Code.Atomic_Load_Store(
         Atomic_Operation => System.Machine_Code.Fence,
         Address          => System.Null_Address,
         Value            => 0,
         Size             => 0
      );
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
      Threshold_Reg := Reg(PLIC_Address(Ctx_Base));
      pragma Assert (Ctx_Base < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
                       "Threshold register address out of bounds");
      -- Initialize the threshold register to 0 (all interrupts enabled)
      Threshold_Reg.all := 0;
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;

      
end Arch.PLIC;