--  arch-plic.ads: Specification of Platform-Level
--  Interrupt Controller (PLIC) utilities.
--  Copyright (C) 2025 scweeks
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with System;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package Arch.PLIC with SPARK_Mode => Off is
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
   --
   --  Ada contracts (Pre/Post conditions) are used to enforce interface expectations.
   ------------------------------------------------------------------------------

   -- Set dynamic configuration. All parameters (except those defined by the ISA)
   -- come from the DTB (or a higher-level configuration module). The parameter
   -- Enabled indicates if the PLIC is supported on this platform.
   procedure Set_PLIC_Configuration (
      Base_Address         : System.Address := System.Storage_Elements.To_Address (16#0C000000#);
      Priority_Offset      : Unsigned_64   := 0;
      Context_Base_Offset  : Unsigned_64   := 16#200000#;
      Context_Stride       : Unsigned_64   := 16#1000#;
      Threshold_Offset     : Unsigned_64   := 0;
      Max_Interrupt_ID     : Unsigned_64   := 1023;
      Max_Harts            : Unsigned_64   := 1;
      Contexts_Per_Hart    : Unsigned_64   := 1;
      Enabled              : Boolean       := True
   )
   with Pre  => True,
        Post => (Get_PLIC_Base = Base_Address) and then
                (Get_Priority_Offset = Priority_Offset) and then
                (Get_Context_Base = Context_Base_Offset) and then
                (Get_Context_Stride = Context_Stride) and then
                (Get_Threshold_Offset = Threshold_Offset) and then
                (Get_Max_Interrupt_ID = Max_Interrupt_ID) and then
                (Get_Max_Harts = Max_Harts) and then
                (Get_Contexts_Per_Hart = Contexts_Per_Hart) and then
                (Is_Enabled = Enabled);

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
                         Context_ID: Unsigned_64 := 0)
      with Inline,
         Pre  => Hart_ID < Get_Max_Harts,
         Post => True;

   function Claim (Hart_ID   : Unsigned_64;
                   Context_ID: Unsigned_64 := 0) return Unsigned_64
      with Inline,
         Pre  => Hart_ID < Get_Max_Harts,
         Post => True;

   procedure Complete (Hart_ID   : Unsigned_64;
                       Context_ID: Unsigned_64 := 0;
                       Interrupt_ID : Unsigned_64)
      with Inline,
         Pre  => (Hart_ID < Get_Max_Harts) and then (Interrupt_ID <= Get_Max_Interrupt_ID),
         Post => True;

   procedure Acknowledge (Hart_ID   : Unsigned_64;
                          Context_ID: Unsigned_64 := 0)
      with Inline,
         Pre  => Hart_ID < Get_Max_Harts,
         Post => True;

   ------------------------------------------------------------------------------
   --  Priority Management
   ------------------------------------------------------------------------------
   procedure Set_Interrupt_Priority (Interrupt_ID : Unsigned_64;
                                     Priority     : Unsigned_64)
      with Inline,
         Pre  => Interrupt_ID <= Get_Max_Interrupt_ID,
         Post => Get_Interrupt_Priority (Interrupt_ID) = Priority;

   function Get_Interrupt_Priority (Interrupt_ID : Unsigned_64) return Unsigned_64
      with Inline,
         Pre  => Interrupt_ID <= Get_Max_Interrupt_ID,
         Post => True;

   ------------------------------------------------------------------------------
   --  Threshold Management
   ------------------------------------------------------------------------------
   procedure Set_Threshold (Hart_ID   : Unsigned_64;
                            Context_ID: Unsigned_64;
                            Threshold : Unsigned_64)
      with Inline,
         Pre  => Hart_ID < Get_Max_Harts,
         Post => Get_Threshold (Hart_ID, Context_ID) = Threshold;

   function Get_Threshold (Hart_ID   : Unsigned_64;
                           Context_ID: Unsigned_64) return Unsigned_64
      with Inline,
         Pre  => Hart_ID < Get_Max_Harts,
         Post => True;

   ------------------------------------------------------------------------------
   --  Dynamic Reconfiguration for Multi-Hart Systems
   ------------------------------------------------------------------------------
   procedure Reset_All with Inline;

   ------------------------------------------------------------------------------
   --  Dynamic Reconfiguration for Multi-Hart Systems (Hot-Reconfiguration)
   ------------------------------------------------------------------------------
   procedure Hot_Reconfigure (
      New_Base_Address         : System.Address;
      New_Priority_Offset      : Unsigned_64;
      New_Context_Base_Offset  : Unsigned_64;
      New_Context_Stride       : Unsigned_64;
      New_Threshold_Offset     : Unsigned_64;
      New_Max_Interrupt_ID     : Unsigned_64;
      New_Max_Harts            : Unsigned_64;
      New_Contexts_Per_Hart    : Unsigned_64
   )
      with Inline,
           Pre  => New_Max_Harts > 0 and then New_Contexts_Per_Hart > 0,
           Post => (Get_PLIC_Base = New_Base_Address) and then
                   (Get_Priority_Offset = New_Priority_Offset) and then
                   (Get_Context_Base = New_Context_Base_Offset) and then
                   (Get_Context_Stride = New_Context_Stride) and then
                   (Get_Threshold_Offset = New_Threshold_Offset) and then
                   (Get_Max_Interrupt_ID = New_Max_Interrupt_ID) and then
                   (Get_Max_Harts = New_Max_Harts) and then
                   (Get_Contexts_Per_Hart = New_Contexts_Per_Hart);

   ------------------------------------------------------------------------------
   --  Memory Barrier
   --  Issues a full fence instruction to ensure ordering of memory operations.
   ------------------------------------------------------------------------------
   procedure Memory_Barrier with Inline;
end Arch.PLIC;