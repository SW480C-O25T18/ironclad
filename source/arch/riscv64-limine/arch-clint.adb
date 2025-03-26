--  arch-exceptions.ads: Specification of Core Local Interruptor (CLINT) utilities.
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

with Arch.CLINT;
with System;
with Interfaces; use Interfaces;
with Ada.Assertions;         -- For contract checking
with System.Machine_Code;    -- For inline assembly fence
with Arch.Debug;             -- For debug printing

package body Arch.CLINT with SPARK_Mode => off is

   ------------------------------------------------------------------------------
   --  Protected Configuration Object for CLINT Settings
   ------------------------------------------------------------------------------
   protected CLINT_Config is
      procedure Set (
         Base_Address     : System.Address;
         MSIP_Offset      : Unsigned_64;
         MTime_Offset     : Unsigned_64;
         MTimecmp_Offset  : Unsigned_64;
         Enabled          : Boolean
      );
      function Get_Base_Address return System.Address;
      function Get_MSIP_Offset return Unsigned_64;
      function Get_MTime_Offset return Unsigned_64;
      function Get_MTimecmp_Offset return Unsigned_64;
      function Is_Enabled return Boolean;
   private
      Base_Address     : System.Address := System'To_Address(16#02000000#);
      MSIP_Offset      : Unsigned_64   := 0;
      MTime_Offset     : Unsigned_64   := 16#BFF8#;
      MTimecmp_Offset  : Unsigned_64   := 16#4000#;
      Enabled          : Boolean       := True;
   end CLINT_Config;

   protected body CLINT_Config is
      procedure Set (
         Base_Address     : System.Address;
         MSIP_Offset      : Unsigned_64;
         MTime_Offset     : Unsigned_64;
         MTimecmp_Offset  : Unsigned_64;
         Enabled          : Boolean
      ) is
      begin
         pragma Assert (Base_Address /= System.Null_Address);
         pragma Assert (MSIP_Offset < 16#1000#);
         pragma Assert (MTime_Offset < 16#1000#);
         pragma Assert (MTimecmp_Offset < 16#1000#);
         pragma Assert (Enabled in Boolean);
         -- The CLINT is only supported if the base address is not null
         -- and the MSIP offset is within the range of the CLINT
         Base_Address    := Base_Address;
         -- MSIP_Offset is per hart, so no range check
         -- 0x0200_0000 + 4 × hartid (per hart)
         -- Triggers a software interrupt on a specific hart by setting bit 0 to 1.
         MSIP_Offset     := MSIP_Offset;  
         -- MTIME is a 64-bit register that is incremented every clock cycle.
         -- Address: 0x0200_BFF8 (global)
         -- Free-running timer incremented by the platform at a fixed frequency.
         -- Read-only (software usually reads this to determine current time)
         MTime_Offset    := MTime_Offset;
         -- MTIMECMP is a 64-bit register that triggers an interrupt when MTIME >= MTIMECMP.
         -- Address: 0x0200_4000 + 8 × hartid (per hart)
         -- Compare value for the timer interrupt.
         MTimecmp_Offset := MTimecmp_Offset;
         -- Enable/Disable the CLINT
         Enabled         := Enabled;
      end Set;

      ------------------------------------------------------------------------------
      --  Protected CLINT Getter functions
      ------------------------------------------------------------------------------
      -- Get CLINT Base Address
      function Get_Base_Address return System.Address is
      begin
         return Base_Address;
      end Get_Base_Address;

      -- Get MSIP (Machine Software Interrupt (per-hart) Offset
      -- Address: 0x0200_0000 + 4 × hartid (per hart)
      function Get_MSIP_Offset return Unsigned_64 is
      begin
         return MSIP_Offset;
      end Get_MSIP_Offset;

      -- Get MTime (Machine Time) Offset
      -- Address: 0x0200_BFF8 (global)
      function Get_MTime_Offset return Unsigned_64 is
      begin
         return MTime_Offset;
      end Get_MTime_Offset;

      -- Get MTimecmp (Machine Time Compare) Offset
      -- Address: 0x0200_4000 + 8 × hartid (per hart)
      function Get_MTimecmp_Offset return Unsigned_64 is
      begin
         return MTimecmp_Offset;
      end Get_MTimecmp_Offset;

      -- Is the CLINT Enabled?
      function Is_Enabled return Boolean is
      begin
         return Enabled;
      end Is_Enabled;

   end CLINT_Config;

   ------------------------------------------------------------------------------
   --  Public CLINT Getter functions
   ------------------------------------------------------------------------------
   -- Get CLINT Base Address
   function Get_CLINT_Base return System.Address is
   begin
      return CLINT_Config.Get_Base_Address;
   end Get_CLINT_Base;

   -- Get MSIP (Machine Software Interrupt (per-hart) Offset
   function Get_MSIP_Offset return Unsigned_64 is
   begin
      return CLINT_Config.Get_MSIP_Offset;
   end Get_MSIP_Offset;

   -- Get MTime (Machine Time) Offset
   function Get_MTime_Offset return Unsigned_64 is
   begin
      return CLINT_Config.Get_MTime_Offset;
   end Get_MTime_Offset;

   -- Get MTimecmp (Machine Time Compare) Offset
   function Get_MTimecmp_Offset return Unsigned_64 is
   begin
      return CLINT_Config.Get_MTimecmp_Offset;
   end Get_MTimecmp_Offset;

   -- Is the CLINT Enabled?
   function CLINT_Enabled return Boolean is
   begin
      return CLINT_Config.Is_Enabled;
   end CLINT_Enabled;

   ------------------------------------------------------------------------------
   --  Volatile Register Access: Define a volatile type.
   ------------------------------------------------------------------------------

   type Reg_Type is new Unsigned_64 with Volatile;
   pragma Volatile (Reg_Type);
   
   
   ------------------------------------------------------------------------------
   --  Software Interrupt Management
   ------------------------------------------------------------------------------



end Arch.CLINT;