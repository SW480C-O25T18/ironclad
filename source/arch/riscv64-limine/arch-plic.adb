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

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with Ada.Assertions;
with System.Machine_Code; use System.Machine_Code;
with Arch.Debug;

package body Arch.PLIC with SPARK_Mode => Off is

   -----------------------------------------------------------------------------
   --  Configuration Variables
   -----------------------------------------------------------------------------
   PLIC_Base_Address         : System.Address := To_Address(16#0C000000#);
   PLIC_Priority_Offset      : Unsigned_64    := 0;
   PLIC_Context_Base_Offset  : Unsigned_64    := 16#200000#;
   PLIC_Context_Stride       : Unsigned_64    := 16#1000#;
   PLIC_Threshold_Offset     : Unsigned_64    := 0;
   PLIC_Max_Interrupt_ID     : Unsigned_64    := 1023;
   PLIC_Max_Harts            : Unsigned_64    := 1;
   PLIC_Contexts_Per_Hart    : Unsigned_64    := 1;
   PLIC_Enabled              : Boolean        := True;

   -----------------------------------------------------------------------------
   --  Getters and Setters
   -----------------------------------------------------------------------------
   function Get_PLIC_Base return System.Address is
   begin
      Arch.Debug.Print("Get_PLIC_Base: Returning PLIC base address");
      return PLIC_Base_Address;
   end Get_PLIC_Base;

   function Get_Priority_Offset return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Priority_Offset: Returning priority offset");
      return PLIC_Priority_Offset;
   end Get_Priority_Offset;

   function Get_Context_Base return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Context_Base: Returning context base offset");
      return PLIC_Context_Base_Offset;
   end Get_Context_Base;

   function Get_Context_Stride return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Context_Stride: Returning context stride");
      return PLIC_Context_Stride;
   end Get_Context_Stride;

   function Get_Threshold_Offset return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Threshold_Offset: Returning threshold offset");
      return PLIC_Threshold_Offset;
   end Get_Threshold_Offset;

   function Get_Max_Interrupt_ID return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Max_Interrupt_ID: Returning max interrupt ID");
      return PLIC_Max_Interrupt_ID;
   end Get_Max_Interrupt_ID;

   function Get_Max_Harts return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Max_Harts: Returning max harts");
      return PLIC_Max_Harts;
   end Get_Max_Harts;

   function Get_Contexts_Per_Hart return Unsigned_64 is
   begin
      Arch.Debug.Print("Get_Contexts_Per_Hart: Returning contexts per hart");
      return PLIC_Contexts_Per_Hart;
   end Get_Contexts_Per_Hart;

   function Is_Enabled return Boolean is
   begin
      Arch.Debug.Print("Is_Enabled: Returning PLIC enabled status");
      return PLIC_Enabled;
   end Is_Enabled;

   procedure Set_PLIC_Configuration (
      Base_Address         : System.Address := To_Address(16#0C000000#);
      Priority_Offset      : Unsigned_64    := 0;
      Context_Base_Offset  : Unsigned_64    := 16#200000#;
      Context_Stride       : Unsigned_64    := 16#1000#;
      Threshold_Offset     : Unsigned_64    := 0;
      Max_Interrupt_ID     : Unsigned_64    := 1023;
      Max_Harts            : Unsigned_64    := 1;
      Contexts_Per_Hart    : Unsigned_64    := 1;
      Enabled              : Boolean        := True
   ) is
   begin
      Arch.Debug.Print("Set_PLIC_Configuration: Setting PLIC configuration");
      PLIC_Base_Address        := Base_Address;
      PLIC_Priority_Offset     := Priority_Offset;
      PLIC_Context_Base_Offset := Context_Base_Offset;
      PLIC_Context_Stride      := Context_Stride;
      PLIC_Threshold_Offset    := Threshold_Offset;
      PLIC_Max_Interrupt_ID    := Max_Interrupt_ID;
      PLIC_Max_Harts           := Max_Harts;
      PLIC_Contexts_Per_Hart   := Contexts_Per_Hart;
      PLIC_Enabled             := Enabled;
      Arch.Debug.Print("Set_PLIC_Configuration: Configuration set successfully");
   end Set_PLIC_Configuration;

   -----------------------------------------------------------------------------
   --  Helper Functions
   -----------------------------------------------------------------------------
   function PLIC_Address (Offset : Unsigned_64) return System.Address is
   begin
      Arch.Debug.Print("PLIC_Address: Calculating address with offset: "
         & Unsigned_64'Image(Offset));
      return To_Address(Address_To_U64(Get_PLIC_Base) + Offset);
   end PLIC_Address;

   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
   type Reg_Ptr is access all Reg_Type;

   function To_Reg_Ptr is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Reg_Ptr
   );

   function Reg (Addr : System.Address) return Reg_Ptr is
   begin
      Arch.Debug.Print("Reg: Converting address to register pointer");
      return To_Reg_Ptr(Addr);
   end Reg;

   procedure Memory_Barrier is
   begin
      Arch.Debug.Print("Memory_Barrier: Executing memory barrier");
      Asm("fence", Clobber => "memory");
      Arch.Debug.Print("Memory_Barrier: Memory barrier executed");
   end Memory_Barrier;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Hart_ID   : Unsigned_64;
                         Context_ID: Unsigned_64 := 0) is
      Ctx_Base      : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : Reg_Ptr;
   begin
      Arch.Debug.Print("Initialize: Starting PLIC initialization for Hart_ID: "
         & Unsigned_64'Image(Hart_ID) & ", Context_ID: " & Unsigned_64'Image(Context_ID));

      if not Is_Enabled then
         Arch.Debug.Print("Initialize: PLIC is disabled. Exiting initialization.");
         return;
      end if;

      declare
         Threshold_Addr : constant System.Address := PLIC_Address(Ctx_Base);
      begin
         Threshold_Reg := Reg(Threshold_Addr);
         Arch.Debug.Print("Initialize: Threshold register address: "
            & Unsigned_64'Image(Address_To_U64(Threshold_Addr)));
      end;

      pragma Assert (Ctx_Base < (Get_Context_Base + ((Hart_ID + 1) * Get_Context_Stride)),
                     "Initialize: Threshold register address out of bounds");

      Arch.Debug.Print("Initialize: Writing 0 to threshold register");
      Threshold_Reg.all := 0;
      Arch.Debug.Print("Initialize: Threshold register initialized to 0");

      Memory_Barrier;

      Arch.Debug.Print("Initialize: PLIC initialization completed for Hart_ID: "
         & Unsigned_64'Image(Hart_ID) & ", Context_ID: " & Unsigned_64'Image(Context_ID));
   end Initialize;

   -----------------------------------------------------------------------------
   --  Claim
   -----------------------------------------------------------------------------
   function Claim (Hart_ID   : Unsigned_64;
                   Context_ID: Unsigned_64 := 0) return Unsigned_64 is
      Ctx_Base   : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Claim_Reg  : Reg_Ptr;
      Claimed_ID : Unsigned_64;
   begin
      Arch.Debug.Print("Claim: Claiming interrupt for Hart_ID: "
         & Unsigned_64'Image(Hart_ID) & ", Context_ID: " & Unsigned_64'Image(Context_ID));

      Claim_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset + 4));
      Claimed_ID := Claim_Reg.all;

      Arch.Debug.Print("Claim: Claimed interrupt ID: " & Unsigned_64'Image(Claimed_ID));
      return Claimed_ID;
   end Claim;

   -----------------------------------------------------------------------------
   --  Complete
   -----------------------------------------------------------------------------
   procedure Complete (Hart_ID   : Unsigned_64;
                       Context_ID: Unsigned_64 := 0;
                       Interrupt_ID : Unsigned_64) is
      Ctx_Base     : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Complete_Reg : Reg_Ptr;
   begin
      Arch.Debug.Print("Complete: Completing interrupt for Hart_ID: "
         & Unsigned_64'Image(Hart_ID) & ", Context_ID: " & Unsigned_64'Image(Context_ID)
         & ", Interrupt_ID: " & Unsigned_64'Image(Interrupt_ID));

      Complete_Reg := Reg(PLIC_Address(Ctx_Base + Get_Threshold_Offset + 4));
      Complete_Reg.all := Interrupt_ID;

      Arch.Debug.Print("Complete: Interrupt ID " & Unsigned_64'Image(Interrupt_ID)
         & " completed for Hart_ID: " & Unsigned_64'Image(Hart_ID)
         & ", Context_ID: " & Unsigned_64'Image(Context_ID));
   end Complete;

end Arch.PLIC;