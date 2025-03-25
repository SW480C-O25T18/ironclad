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
with Interfaces; use Interfaces;

package Arch.PLIC is
   ------------------------------------------------------------------------------
   --  Arch.PLIC
   --
   --  This package encapsulates the Platform-Level Interrupt Controller (PLIC)
   --  for RISCV64 and supports full dynamic configuration based on the device
   --  tree (DTB). All parameters not dictated by the RISCV64 ISA are set via
   --  Set_PLIC_Configuration. Default values are provided so that basic functionality
   --  is available even if DTB parsing is incomplete.
   --
   --  A Boolean flag (Enabled) is maintained to indicate whether the PLIC is
   --  supported on the platform. If not, every PLIC routine will print a debug
   --  message and fail gracefully.
   ------------------------------------------------------------------------------
   
   -- Set dynamic configuration. All parameters (except those defined by the ISA)
   -- come from the DTB (or a higher-level configuration module). The parameter
   -- Enabled indicates if the PLIC is supported on this platform.
   procedure Set_PLIC_Configuration (
     Base_Address         : System.Address := System'To_Address(16#0C000000#);
     Priority_Offset      : Unsigned_64   := 0;
     Context_Base_Offset  : Unsigned_64   := 16#200000#;
     Context_Stride       : Unsigned_64   := 16#1000#;
     Threshold_Offset     : Unsigned_64   := 0;
     Max_Interrupt_ID     : Unsigned_64   := 1023;
     Max_Harts            : Unsigned_64   := 1;
     Contexts_Per_Hart    : Unsigned_64   := 1;
     Enabled              : Boolean       := True
   );

   function Get_PLIC_Base return System.Address;
   function Get_Priority_Offset return Unsigned_64;
   function Get_Context_Base return Unsigned_64;
   function Get_Context_Stride return Unsigned_64;
   function Get_Threshold_Offset return Unsigned_64;
   function Get_Max_Interrupt_ID return Unsigned_64;
   function Get_Max_Harts return Unsigned_64;
   function Get_Contexts_Per_Hart return Unsigned_64;
   function Is_Enabled return Boolean;

   ------------------------------------------------------------------------------
   --  PLIC Functions for a given Hart and Context (Context_ID defaults to 0)
   ------------------------------------------------------------------------------
   procedure Initialize (Hart_ID   : Unsigned_64;
                         Context_ID: Unsigned_64 := 0);

   function Claim (Hart_ID   : Unsigned_64;
                   Context_ID: Unsigned_64 := 0) return Unsigned_64;

   procedure Complete (Hart_ID   : Unsigned_64;
                       Context_ID: Unsigned_64 := 0;
                       Interrupt_ID : Unsigned_64);

   procedure Acknowledge (Hart_ID   : Unsigned_64;
                          Context_ID: Unsigned_64 := 0);

   ------------------------------------------------------------------------------
   --  Priority Management
   ------------------------------------------------------------------------------
   procedure Set_Interrupt_Priority (Interrupt_ID : Unsigned_64;
                                     Priority     : Unsigned_64);
   function Get_Interrupt_Priority (Interrupt_ID : Unsigned_64) return Unsigned_64;

   ------------------------------------------------------------------------------
   --  Threshold Management
   ------------------------------------------------------------------------------
   procedure Set_Threshold (Hart_ID   : Unsigned_64;
                            Context_ID: Unsigned_64;
                            Threshold : Unsigned_64);
   function Get_Threshold (Hart_ID   : Unsigned_64;
                           Context_ID: Unsigned_64) return Unsigned_64;

   ------------------------------------------------------------------------------
   --  Dynamic Reconfiguration for Multi-Hart Systems
   ------------------------------------------------------------------------------
   procedure Reset_All;

   ------------------------------------------------------------------------------
   --  Memory Barrier
   --
   --  Issues a full fence instruction to ensure ordering of memory operations.
   ------------------------------------------------------------------------------
   procedure Memory_Barrier;
end Arch.PLIC;
