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

with System;
with Interfaces; use Interfaces;

package Arch.CLINT with SPARK_Mode => Off is
   ------------------------------------------------------------------------------
   --  Arch.CLINT
   --
   --  This package encapsulates the Core Local Interruptor (CLINT) for RISCV64.
   --  It supports dynamic configuration based on DTB (or other boot configuration)
   --  parameters. All configuration parameters not dictated by the RISCV64 ISA are
   --  set via Set_CLINT_Configuration. Default values are provided to allow basic
   --  functionality even if DTB parsing is incomplete.
   --
   --  The CLINT manages:
   --    - Software interrupts (msip registers) for each hart:
   --         Address = CLINT_Base + MSIP_Offset + (Hart_ID * 4)
   --
   --    - Timer registers:
   --         mtime (global, 64-bit) at CLINT_Base + MTime_Offset
   --         mtimecmp (per hart, 64-bit) at
   --             CLINT_Base + MTimecmp_Offset + (Hart_ID * 8)
   --
   --  A Boolean flag (Enabled) indicates whether the CLINT is supported. When
   --  disabled, all functions report the lack of support and return safe defaults.
   ------------------------------------------------------------------------------
   with System;
with Interfaces; use Interfaces;

package Arch.CLINT with SPARK_Mode => Off is
   ------------------------------------------------------------------------------
   --  Arch.CLINT
   --
   --  This package encapsulates the Core Local Interruptor (CLINT) for RISCV64.
   --  All configuration parameters (other than those defined by the RISCV64 ISA)
   --  are set dynamically via Set_CLINT_Configuration. A Boolean flag (Enabled)
   --  indicates whether the CLINT is supported on the platform.
   ------------------------------------------------------------------------------
   procedure Set_CLINT_Configuration (
     Base_Address     : System.Address := System'To_Address(16#02000000#);
     MSIP_Offset      : Unsigned_64   := 0;
     MTime_Offset     : Unsigned_64   := 16#BFF8#;
     MTimecmp_Offset  : Unsigned_64   := 16#4000#;
     Enabled          : Boolean       := True
   )
   with Pre  => True,
        Post => (Get_CLINT_Base = Base_Address) and then 
                (Get_MSIP_Offset = MSIP_Offset) and then 
                (Get_MTime_Offset = MTime_Offset) and then 
                (Get_MTimecmp_Offset = MTimecmp_Offset) and then 
                (CLINT_Enabled = Enabled);
   
   function Get_CLINT_Base return System.Address;
   function Get_MSIP_Offset return Unsigned_64;
   function Get_MTime_Offset return Unsigned_64;
   function Get_MTimecmp_Offset return Unsigned_64;
   function CLINT_Enabled return Boolean;

   ------------------------------------------------------------------------------
   --  Software Interrupt Management
   ------------------------------------------------------------------------------
   procedure Set_Software_Interrupt (Hart_ID : Unsigned_64; Value : Boolean)
      with Inline,
           Pre  => Hart_ID >= 0,
           Post => True;

   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64)
      with Inline,
           Pre  => Hart_ID >= 0,
           Post => True;

   function Read_Software_Interrupt (Hart_ID : Unsigned_64) return Boolean
      with Inline,
           Pre  => Hart_ID >= 0,
           Post => True;

   ------------------------------------------------------------------------------
   --  Timer Management
   ------------------------------------------------------------------------------
   function Get_MTime return Unsigned_64
      with Inline,
           Post => True;

   procedure Set_Timer_Compare (Hart_ID : Unsigned_64; Time : Unsigned_64)
      with Inline,
           Pre  => Hart_ID >= 0,
           Post => Get_Timer_Compare(Hart_ID) = Time;

   function Get_Timer_Compare (Hart_ID : Unsigned_64) return Unsigned_64
      with Inline,
           Pre  => Hart_ID >= 0,
           Post => True;

   ------------------------------------------------------------------------------
   --  Memory Barrier
   ------------------------------------------------------------------------------
   procedure Memory_Barrier with Inline;

end Arch.CLINT;
   