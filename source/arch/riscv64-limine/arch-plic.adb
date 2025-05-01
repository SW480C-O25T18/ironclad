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
with Ada.Assertions;         -- For contract checking
with System.Machine_Code;    -- For inline assembly fence
with Arch.Debug;             -- For debug printing       

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
         Threshold_Offset     : Unsigned_64;
         Max_Interrupt_ID     : Unsigned_64;
         Max_Harts            : Unsigned_64;
         Contexts_Per_Hart    : Unsigned_64;
         Enabled              : Boolean
      );
      function Get_Base return System.Address;
      function Get_Priority_Offset return Unsigned_64;
      function Get_Context_Base return Unsigned_64;
      function Get_Context_Stride return Unsigned_64;
      function Get_Threshold_Offset return Unsigned_64;
      function Get_Max_Interrupt_ID return Unsigned_64;
      function Get_Max_Harts return Unsigned_64;
      function Get_Contexts_Per_Hart return Unsigned_64;
      function Get_Enabled return Boolean;
   private
      Base : System.Address := System'To_Address(16#0C000000#);
      Priority_Offset : Unsigned_64 := 0;
      Context_Base : Unsigned_64 := 16#200000#;
      Context_Stride : Unsigned_64 := 16#1000#;
      Threshold_Offset : Unsigned_64 := 0;
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
         Threshold_Offset     : Unsigned_64;
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
         Threshold_Offset := Threshold_Offset;
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

      function Get_Threshold_Offset return Unsigned_64 is
      begin
         return Threshold_Offset;
      end Get_Threshold_Offset;

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
      Threshold_Offset     : Unsigned_64;
      Max_Interrupt_ID     : Unsigned_64;
      Max_Harts            : Unsigned_64;
      Contexts_Per_Hart    : Unsigned_64;
      Enabled              : Boolean := True
   ) is
   begin
      PLIC_Config.Set(Base_Address, Priority_Offset,
                      Context_Base_Offset, Context_Stride, Threshold_Offset,
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

   function Get_Threshold_Offset return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Threshold_Offset: " & Unsigned_64'Image(PLIC_Config.Get_Threshold_Offset));
      return PLIC_Config.Get_Threshold_Offset;
   end Get_Threshold_Offset;

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
   
   -- System address access conversion to volatile register pointer
   function Reg (Abs_Addr : System.Address) return Reg_Ptr is
   begin
      Arch.Debug.Print("Reg: Address: " & Unsigned_64'Image(To_Integer(Abs_Addr)));
      Arch.Debug.Print("Reg: Address End");
      return Reg_Ptr(Abs_Addr);
   end Reg;

   ------------------------------------------------------------------------------
   --  Memory_Barrier
   --  Issues a RISC-V fence instruction to enforce ordering of memory operations.
   ------------------------------------------------------------------------------
   procedure Memory_Barrier is
   begin
      Arch.Debug.Print("Memory barrier Start");
      Asm ("fence",
         Volatile => True,
         Clobber => "memory");
      Arch.Debug.Print("Memory barrier End");
   end Memory_Barrier;

   ------------------------------------------------------------------------------
   -- Check if PLIC is enabled. If not, print a debug message and return False.
   ------------------------------------------------------------------------------
   function Check_PLIC_Supported return Boolean is
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
      Claim_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset + 4));
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
      Claim_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset + 4));
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
   --  Acknowledge
   --  Acknowledges the completion of an interrupt for a given Hart and Context.
   ------------------------------------------------------------------------------
   procedure Acknowledge (Hart_ID   : Unsigned_64;
                          Context_ID: Unsigned_64 := 0) is
      Int_ID : Unsigned_64;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Acknowledge: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Acknowledge: Start");
      Int_ID := Claim(Hart_ID, Context_ID);
      Arch.Debug.Print("Acknowledge: Acknowledging interrupt ID: " & Unsigned_64'Image(Int_ID));
      -- Acknowledge the completion of the interrupt by writing the claimed ID back to the register
      if Int_ID /= 0 then
         Complete(Hart_ID, Context_ID, Int_ID);
      end if;
      Arch.Debug.Print("Acknowledge: End");
   end Acknowledge;

   ------------------------------------------------------------------------------
   --  Set_Interrupt_Priority
   --  Each interrupt source has an associated priority stored at:
   --      (PLIC base + Priority_Offset) + (Interrupt_ID * 4)
   --  The register is 32 bits wide.
   ------------------------------------------------------------------------------
   procedure Set_Interrupt_Priority (Interrupt_ID : Unsigned_64;
                                     Priority     : Unsigned_64) is
      Reg_Addr : constant System.Address;
      Priority_Reg : Reg_Ptr;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Set_Interrupt_Priority: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Set_Interrupt_Priority: Start");
      pragma Assert (Interrupt_ID <= Get_Max_Interrupt_ID, "Interrupt_ID out of range");
      -- Calculate the address of the priority register for the interrupt source
      Arch.Debug.Print("Set_Interrupt_Priority: Calculating priority register address");
      Reg_Addr := PLIC_Address(Get_Priority_Offset + (Interrupt_ID * 4));
      Arch.Debug.Print("Set_Interrupt_Priority: Priority register address: " & Unsigned_64'Image(To_Integer(Reg_Addr)));
      -- Access the priority register using a volatile pointer
      Arch.Debug.Print("Set_Interrupt_Priority: Accessing priority register");
      Priority_Reg := Reg(Reg_Addr);
      Arch.Debug.Print("Set_Interrupt_Priority: Priority register address: " & Unsigned_64'Image(Priority_Reg'Address));
      -- Set the priority of the interrupt source
      Arch.Debug.Print("Set_Interrupt_Priority: Writing priority to register");
      Priority_Reg.all := Priority;
      Arch.Debug.Print("Set_Interrupt_Priority: Writing priority End");
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;
      Arch.Debug.Print("Set_Interrupt_Priority: End");
   end Set_Interrupt_Priority;

   ------------------------------------------------------------------------------
   --  Get_Interrupt_Priority
   --  Returns the priority of the specified interrupt source.
   ------------------------------------------------------------------------------
   function Get_Interrupt_Priority (Interrupt_ID : Unsigned_64) return Unsigned_64 is
      Reg_Addr : constant System.Address;
      Priority_Reg : Reg_Ptr;
      Priority : Unsigned_64;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Get_Interrupt_Priority: PLIC is disabled.");
         return 0;
      end if;
      Arch.Debug.Print("Get_Interrupt_Priority: Start");
      pragma Assert (Interrupt_ID <= Get_Max_Interrupt_ID, "Interrupt_ID out of range");
      -- Calculate the address of the priority register for the interrupt source
      Arch.Debug.Print("Get_Interrupt_Priority: Calculating priority register address");
      Reg_Addr := PLIC_Address(Get_Priority_Offset + (Interrupt_ID * 4));
      Arch.Debug.Print("Get_Interrupt_Priority: Priority register address: " & Unsigned_64'Image(To_Integer(Reg_Addr)));
      -- Access the priority register using a volatile pointer
      Arch.Debug.Print("Get_Interrupt_Priority: Accessing priority register");
      Priority_Reg := Reg(Reg_Addr);
      Arch.Debug.Print("Get_Interrupt_Priority: Priority register address: " & Unsigned_64'Image(Priority_Reg'Address));
      -- Get the priority of the interrupt source
      Arch.Debug.Print("Get_Interrupt_Priority: Reading priority from register");
      Priority := Priority_Reg.all;
      Arch.Debug.Print("Get_Interrupt_Priority: Priority: " & Unsigned_64'Image(Priority));
      Arch.Debug.Print("Get_Interrupt_Priority: End");
      return Priority;
   end Get_Interrupt_Priority;

   ------------------------------------------------------------------------------
   --  Threshold Management
   --  The threshold register for a given Hart and Context is located at the start
   --  of the context region.
   ------------------------------------------------------------------------------
   ------------------------------------------------------------------------------
   --  Set_Threshold
   --  Each Hart has a threshold register that determines the minimum priority
   --  level of interrupts that can be claimed.
   --  The threshold register is located at:
   --      (PLIC base + Context_Base + (Hart_ID * Context_Stride)) + Threshold_Offset
   --  The register is 32 bits wide.
   ------------------------------------------------------------------------------
   procedure Set_Threshold (Hart_ID   : Unsigned_64;
                           Context_ID: Unsigned_64;
                           Threshold : Unsigned_64) is
      Ctx_Base : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : Reg_Ptr;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Set_Threshold: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Set_Threshold: Start");
      Threshold_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset));
      Arch.Debug.Print("Set_Threshold: Threshold register address: " & Unsigned_64'Image(Threshold_Reg'Address));
      pragma Assert (Ctx_Base + Get_Threshold_Offset < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
               "Threshold register address out of bounds");
      -- Set the threshold for the Hart
      Arch.Debug.Print("Set_Threshold: Writing threshold to register");
      Threshold_Reg.all := Threshold;
      Arch.Debug.Print("Set_Threshold: Writing threshold End");
      -- Memory barrier to ensure ordering of memory operations
      Memory_Barrier;
      Arch.Debug.Print("Set_Threshold: End");
   end Set_Threshold;

   ------------------------------------------------------------------------------
   --  Get_Threshold
   --  Returns the threshold value for the specified Hart and Context.
   ------------------------------------------------------------------------------
   function Get_Threshold (Hart_ID   : Unsigned_64;
                          Context_ID: Unsigned_64) return Unsigned_64 is
      Ctx_Base : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : Reg_Ptr;
      Threshold : Unsigned_64;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Get_Threshold: PLIC is disabled.");
         return 0;
      end if;
      Arch.Debug.Print("Get_Threshold: Start");
      Threshold_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset));
      Arch.Debug.Print("Get_Threshold: Threshold register address: " & Unsigned_64'Image(Threshold_Reg'Address));
      pragma Assert (Ctx_Base + Get_Threshold_Offset < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
               "Threshold register address out of bounds");
      -- Get the threshold for the Hart
      Arch.Debug.Print("Get_Threshold: Reading threshold from register");
      Threshold := Threshold_Reg.all;
      Arch.Debug.Print("Get_Threshold: Threshold: " & Unsigned_64'Image(Threshold));
      Arch.Debug.Print("Get_Threshold: End");
      return Threshold;
   end Get_Threshold;

   ------------------------------------------------------------------------------
   --  Reset_All
   --  Resets all PLIC registers to their default values.
   ------------------------------------------------------------------------------
   procedure Reset_All is
      Max_Harts         : constant Unsigned_64 := Get_Max_Harts;
      Contexts_Per_Hart : constant Unsigned_64 := Get_Contexts_Per_Hart;
      Max_Int           : constant Unsigned_64 := Get_Max_Interrupt_ID;
      Interrupt_ID      : Unsigned_64;
      Hart              : Unsigned_64;
      Context           : Unsigned_64;
   begin
      if not Is_Enabled then
         Arch.Debug.Print("Reset_All: PLIC is disabled.");
         return;
      end if;
      Arch.Debug.Print("Reset_All: Start");
      -- Reset the threshold registers for all Harts and Contexts
      Arch.Debug.Print("Reset_All: Resetting threshold registers");
      for Hart in 0 .. Max_Harts - 1 loop
         for Context in 0 .. Contexts_Per_Hart - 1 loop
            Set_Threshold(Hart, Context, 0);
         end loop;
      end loop;
      -- Reset the priority registers for all interrupt sources
      Arch.Debug.Print("Reset_All: Resetting priority registers");
      for Interrupt_ID in 0 .. Max_Int loop
         Set_Interrupt_Priority(Interrupt_ID, 0);
      end loop;
      Arch.Debug.Print("Reset_All: End");
   end Reset_All;
end Arch.PLIC;