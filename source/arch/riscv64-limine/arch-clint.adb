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
   --  Used for memory-mapped register accesses.
   ------------------------------------------------------------------------------
   -- Volatile register type
   type Reg_Type is new Unsigned_64;
   pragma Volatile (Reg_Type);
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
      System.Machine_Code.Fence;
      Arch.Debug.Print("Memory barrier End");
   end Memory_Barrier;
   
   ------------------------------------------------------------------------------
   --  Software Interrupt Management (msip registers)
   --  Each hart's msip register is located at:
   --      CLINT_Base + MSIP_Offset + (Hart_ID * 4)
   ------------------------------------------------------------------------------

   -- Set Software Interrupt
   procedure Set_Software_Interrupt (Hart_ID : Unsigned_64; Value : Boolean) is
      type MSIP_Type is new Unsigned_32;
      pragma Volatile(MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base + To_Address(Get_MSIP_Offset + (Hart_ID * 4));
      MSIP_Reg : MSIP_Ptr := MSIP_Ptr(Addr);
   begin
      Arch.Debug.Print ("Set_Software_Interrupt: Starting Set Software Interrupt");
      Arch.Debug.Print ("Set_Software_Interrupt: Hart ID: " & Unsigned_64'Image(Hart_ID));
      if not CLINT_Enabled then
         Arch.Debug.Print("Set_Software_Interrupt: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print("Set_Software_Interrupt: MSIP Register Address: " & Unsigned_64'Image(To_Integer(MSIP_Reg'Address)));
      Arch.Debug.Print ("Set_Software_Interrupt: Value: " & Boolean'Image(Value));
      if Value then
         Arch.Debug.Print("Set_Software_Interrupt: Setting Software Interrupt");
         MSIP_Reg.all := 1;
      else
         Arch.Debug.Print("Set_Software_Interrupt: Clearing Software Interrupt");
         MSIP_Reg.all := 0;
      end if;
      Memory_Barrier;
      Arch.Debug.Print ("Set_Software_Interrupt: Ending Set Software Interrupt");   
   end Set_Software_Interrupt;

   -- Clear Software Interrupt
   procedure Clear_Software_Interrupt (Hart_ID : Unsigned_64) is
   begin
      Arch.Debug.Print ("Clear_Software_Interrupt: Starting Clear Software Interrupt");
      if not CLINT_Enabled then
         Arch.Debug.Print("Clear_Software_Interrupt: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print ("Clear_Software_Interrupt: Hart ID: " & Unsigned_64'Image(Hart_ID));
      Arch.Debug.Print ("Clear_Software_Interrupt: Value: False");
      Set_Software_Interrupt(Hart_ID, False);
      Arch.Debug.Print ("Clear_Software_Interrupt: Ending Clear Software Interrupt");
   end Clear_Software_Interrupt;

   -- Read Software Interrupt
   function Read_Software_Interrupt (Hart_ID : Unsigned_64) return Boolean is
      type MSIP_Type is new Unsigned_32;
      pragma Volatile(MSIP_Type);
      type MSIP_Ptr is access all MSIP_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base + To_Address(Get_MSIP_Offset + (Hart_ID * 4));
      MSIP_Reg : MSIP_Ptr := MSIP_Ptr(Addr);
   begin
      Arch.Debug.Print ("Read_Software_Interrupt: Starting Read Software Interrupt");
      if not CLINT_Enabled then
         Arch.Debug.Print("Read_Software_Interrupt: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print ("Read_Software_Interrupt: Hart ID: " & Unsigned_64'Image(Hart_ID));
      Arch.Debug.Print("Read_Software_Interrupt: MSIP Register Address: " & Unsigned_64'Image(To_Integer(MSIP_Reg'Address)));
      Arch.Debug.Print ("Read_Software_Interrupt: Ending Read Software Interrupt");
      return MSIP_Reg.all /= 0;
   end Read_Software_Interrupt;

   ------------------------------------------------------------------------------
   --  Timer Management
   --  mtime (global, 64-bit) is located at:
   --      CLINT_Base + MTime_Offset
   --  mtimecmp (per hart, 64-bit) is located at:
   --      CLINT_Base + MTimecmp_Offset + (Hart_ID * 8)
   ------------------------------------------------------------------------------

   -- Get MTime
   function Get_MTime return Unsigned_64 is
      type Timer_Type is new Unsigned_64;
      pragma Volatile(Timer_Type);
      type Timer_Ptr is access all Timer_Type;
      Addr : constant System.Address := Get_CLINT_Base + To_Address(Get_MTime_Offset);
      Timer_Reg : Timer_Ptr := Timer_Ptr(Addr);
   begin
      Arch.Debug.Print ("Get_MTime: Starting Get MTime");
      if not CLINT_Enabled then
         Arch.Debug.Print("Get_MTime: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print("Get_MTime: MTime Register Address: " & Unsigned_64'Image(To_Integer(Timer_Reg'Address)));
      Arch.Debug.Print ("Get_MTime: Ending Get MTime");
      return Timer_Reg.all;
   end Get_MTime;

   -- Set Timer Compare
   procedure Set_Timer_Compare (Hart_ID : Unsigned_64; Time : Unsigned_64) is
      type Timer_Type is new Unsigned_64;
      pragma Volatile(Timer_Type);
      type Timer_Ptr is access all Timer_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base + To_Address(Get_MTimecmp_Offset + (Hart_ID * 8));
      Timer_Reg : Timer_Ptr := Timer_Ptr(Addr);
   begin
      Arch.Debug.Print ("Set_Timer_Compare: Starting Set Timer Compare");
      if not CLINT_Enabled then
         Arch.Debug.Print("Set_Timer_Compare: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print ("Set_Timer_Compare: Hart ID: " & Unsigned_64'Image(Hart_ID));
      Arch.Debug.Print ("Set_Timer_Compare: Time: " & Unsigned_64'Image(Time));
      Arch.Debug.Print("Set_Timer_Compare: Timer Register Address: " & Unsigned_64'Image(To_Integer(Timer_Reg'Address)));
      Timer_Reg.all := Time;
      Memory_Barrier;
      Arch.Debug.Print ("Set_Timer_Compare: Ending Set Timer Compare");
   end Set_Timer_Compare;

   -- Get Timer Compare
   function Get_Timer_Compare (Hart_ID : Unsigned_64) return Unsigned_64 is
      type Timer_Type is new Unsigned_64;
      pragma Volatile(Timer_Type);
      type Timer_Ptr is access all Timer_Type;
      Addr : constant System.Address :=
         Get_CLINT_Base + To_Address(Get_MTimecmp_Offset + (Hart_ID * 8));
      Timer_Reg : Timer_Ptr := Timer_Ptr(Addr);
   begin
      Arch.Debug.Print ("Get_Timer_Compare: Starting Get Timer Compare");
      if not CLINT_Enabled then
         Arch.Debug.Print("Get_Timer_Compare: CLINT is disabled.");
         return;
      end if;
      Arch.Debug.Print ("Get_Timer_Compare: Hart ID: " & Unsigned_64'Image(Hart_ID));
      Arch.Debug.Print("Get_Timer_Compare: Timer Register Address: " & Unsigned_64'Image(To_Integer(Timer_Reg'Address)));
      Arch.Debug.Print ("Get_Timer_Compare: Ending Get Timer Compare");
      return Timer_Reg.all;
   end Get_Timer_Compare;

end Arch.CLINT;