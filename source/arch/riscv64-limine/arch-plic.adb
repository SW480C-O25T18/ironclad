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

   
end Arch.PLIC;