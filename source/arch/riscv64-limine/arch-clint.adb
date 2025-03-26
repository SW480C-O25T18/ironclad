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

end Arch.CLINT;