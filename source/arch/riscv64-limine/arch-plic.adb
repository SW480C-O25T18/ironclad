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

pragma SPARK_Mode (Off);

with System;              use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces;          use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with Arch.Debug;
with Ada.Unchecked_Conversion;

package body Arch.PLIC is

   -----------------------------------------------------------------------------
   --  Configuration Variables
   -----------------------------------------------------------------------------
   PLIC_Base_Address         : constant System.Address := To_Address(16#0C000000#);
   PLIC_Priority_Offset      : constant Unsigned_64    := 0;
   PLIC_Context_Base_Offset  : constant Unsigned_64    := 16#200000#;
   PLIC_Context_Stride       : constant Unsigned_64    := 16#1000#;
   PLIC_Threshold_Offset     : constant Unsigned_64    := 0;
   PLIC_Max_Interrupt_ID     : constant Unsigned_64    := 1023;
   PLIC_Max_Harts            : constant Unsigned_64    := 1;
   PLIC_Contexts_Per_Hart    : constant Unsigned_64    := 1;
   PLIC_Enabled              : Boolean                 := True;

   -----------------------------------------------------------------------------
   --  Helper Functions
   -----------------------------------------------------------------------------

   -- Convert System.Address to Unsigned_64
   function Address_To_U64 is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Unsigned_64);

   -- Convert Unsigned_64 to System.Address
   function U64_To_Address is new Ada.Unchecked_Conversion(
      Source => Unsigned_64,
      Target => System.Address);

   -- Calculate the context offset for a given hart and context ID
   function Context_Offset (
      Hart_ID    : Unsigned_64;
      Context_ID : Unsigned_64
   ) return Unsigned_64 is
   begin
      Arch.Debug.Print(
         "Context_Offset: Calculating context offset for Hart_ID=" &
         Unsigned_64'Image(Hart_ID) & ", Context_ID=" & Unsigned_64'Image(Context_ID));
      return PLIC_Context_Base_Offset +
             (Hart_ID * PLIC_Context_Stride) +
             (Context_ID * PLIC_Context_Stride);
   end Context_Offset;

   -- Calculate the PLIC register address for a given offset
   function PLIC_Address (Offset : Unsigned_64) return System.Address is
   begin
      Arch.Debug.Print("PLIC_Address: Calculating address with offset=" &
                       Unsigned_64'Image(Offset));
      return U64_To_Address(Address_To_U64(PLIC_Base_Address) + Offset);
   end PLIC_Address;

   -----------------------------------------------------------------------------
   --  Initialize
   -----------------------------------------------------------------------------
   procedure Initialize (Hart_ID : Unsigned_64; Context_ID : Unsigned_64 := 0) is
      Ctx_Base      : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : access Unsigned_64 := To_Address(Ctx_Base + PLIC_Threshold_Offset);
   begin
      Arch.Debug.Print("Initialize: Initializing PLIC for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID));

      if not PLIC_Enabled then
         Arch.Debug.Print("Initialize: PLIC is disabled. Exiting initialization.");
         return;
      end if;

      -- Set the threshold register to 0
      Threshold_Reg.all := 0;

      Arch.Debug.Print("Initialize: PLIC initialized successfully for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID));
   end Initialize;

   -----------------------------------------------------------------------------
   --  Claim
   -----------------------------------------------------------------------------
   function Claim (Hart_ID : Unsigned_64; Context_ID : Unsigned_64 := 0) return Unsigned_64 is
      Ctx_Base   : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Claim_Reg  : access Unsigned_64 := To_Address(Ctx_Base + 4);
   begin
      Arch.Debug.Print("Claim: Claiming interrupt for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID));
      return Claim_Reg.all;
   end Claim;

   -----------------------------------------------------------------------------
   --  Complete
   -----------------------------------------------------------------------------
   procedure Complete (
      Hart_ID      : Unsigned_64;
      Context_ID   : Unsigned_64 := 0;
      Interrupt_ID : Unsigned_64
   ) is
      Ctx_Base     : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Complete_Reg : access Unsigned_64 := To_Address(Ctx_Base + 4);
   begin
      Arch.Debug.Print("Complete: Completing interrupt for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID) & ", Interrupt_ID=" &
                       Unsigned_64'Image(Interrupt_ID));
      Complete_Reg.all := Interrupt_ID;
   end Complete;

   -----------------------------------------------------------------------------
   --  Set_Interrupt_Priority
   -----------------------------------------------------------------------------
   procedure Set_Interrupt_Priority (Interrupt_ID : Unsigned_64; Priority : Unsigned_64) is
      Priority_Reg : access Unsigned_64 := To_Address(PLIC_Priority_Offset + (Interrupt_ID * 4));
   begin
      Arch.Debug.Print("Set_Interrupt_Priority: Setting priority for Interrupt_ID=" &
                       Unsigned_64'Image(Interrupt_ID) & " to " &
                       Unsigned_64'Image(Priority));
      Priority_Reg.all := Priority;
   end Set_Interrupt_Priority;

   -----------------------------------------------------------------------------
   --  Get_Interrupt_Priority
   -----------------------------------------------------------------------------
   function Get_Interrupt_Priority (Interrupt_ID : Unsigned_64) return Unsigned_64 is
      Priority_Reg : access Unsigned_64 := To_Address(PLIC_Priority_Offset + (Interrupt_ID * 4));
   begin
      Arch.Debug.Print("Get_Interrupt_Priority: Getting priority for Interrupt_ID=" &
                       Unsigned_64'Image(Interrupt_ID));
      return Priority_Reg.all;
   end Get_Interrupt_Priority;

   -----------------------------------------------------------------------------
   --  Set_Threshold
   -----------------------------------------------------------------------------
   procedure Set_Threshold (Hart_ID : Unsigned_64; Context_ID : Unsigned_64 := 0; Threshold : Unsigned_64) is
      Ctx_Base      : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : access Unsigned_64 := To_Address(Ctx_Base + PLIC_Threshold_Offset);
   begin
      Arch.Debug.Print("Set_Threshold: Setting threshold for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID) & " to " &
                       Unsigned_64'Image(Threshold));
      Threshold_Reg.all := Threshold;
   end Set_Threshold;

   -----------------------------------------------------------------------------
   --  Get_Threshold
   -----------------------------------------------------------------------------
   function Get_Threshold (Hart_ID : Unsigned_64; Context_ID : Unsigned_64 := 0) return Unsigned_64 is
      Ctx_Base      : constant Unsigned_64 := Context_Offset(Hart_ID, Context_ID);
      Threshold_Reg : access Unsigned_64 := To_Address(Ctx_Base + PLIC_Threshold_Offset);
   begin
      Arch.Debug.Print("Get_Threshold: Getting threshold for Hart_ID=" &
                       Unsigned_64'Image(Hart_ID) & ", Context_ID=" &
                       Unsigned_64'Image(Context_ID));
      return Threshold_Reg.all;
   end Get_Threshold;

   -----------------------------------------------------------------------------
   --  Reset_All
   -----------------------------------------------------------------------------
   procedure Reset_All is
   begin
      Arch.Debug.Print("Reset_All: Resetting all PLIC configurations");

      -- Reset all priorities
      for Interrupt_ID in 0 .. PLIC_Max_Interrupt_ID loop
         Set_Interrupt_Priority(Interrupt_ID, 0);
      end loop;

      -- Reset all thresholds
      for Hart_ID in 0 .. PLIC_Max_Harts - 1 loop
         for Context_ID in 0 .. PLIC_Contexts_Per_Hart - 1 loop
            Set_Threshold(Hart_ID, Context_ID, 0);
         end loop;
      end loop;

      Arch.Debug.Print("Reset_All: All PLIC configurations reset successfully");
   end Reset_All;

   -----------------------------------------------------------------------------
   --  Hot_Reconfigure
   -----------------------------------------------------------------------------
   procedure Hot_Reconfigure is
   begin
      Arch.Debug.Print("Hot_Reconfigure: Starting hot reconfiguration of PLIC");

      -- Example: Reconfigure priorities and thresholds dynamically
      for Interrupt_ID in 0 .. PLIC_Max_Interrupt_ID loop
         Set_Interrupt_Priority(Interrupt_ID, Interrupt_ID mod 8); -- Example priority logic
      end loop;

      for Hart_ID in 0 .. PLIC_Max_Harts - 1 loop
         for Context_ID in 0 .. PLIC_Contexts_Per_Hart - 1 loop
            Set_Threshold(Hart_ID, Context_ID, 4); -- Example threshold logic
         end loop;
      end loop;

      Arch.Debug.Print("Hot_Reconfigure: Hot reconfiguration completed successfully");
   end Hot_Reconfigure;

end Arch.PLIC;