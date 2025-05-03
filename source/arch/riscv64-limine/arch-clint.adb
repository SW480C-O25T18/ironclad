--  arch-clint.adb: Implementation of Core Local Interruptor (CLINT) utilities.
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces;                use Interfaces;
with System;                    use System;
with System.Storage_Elements;   use System.Storage_Elements;
with Arch.CPU;                  use Arch.CPU;
with Arch.SBI;                  use Arch.SBI;
with Arch.MMU;                  use Arch.MMU;
with Arch.DTB;                  use Arch.DTB;
with Arch.Debug;                use Arch.Debug;

package body Arch.CLINT is

   ----------------------------------------------------------------------------
   --  CLINT availability: prefer SBI, fallback MMIO
   ----------------------------------------------------------------------------
   function CLINT_Enabled return Boolean is
   begin
      return Probe_Extension (Extension_Timer)
         or else Clint_Base /= Null_Address;
   end CLINT_Enabled;

   ----------------------------------------------------------------------------
   --  Global configuration: map MMIO or rely on SBI
   ----------------------------------------------------------------------------
   procedure Set_CLINT_Configuration is
      Node    : DTB_Node_Access := Find_Node_By_Compatible ("riscv,clint0");
      Regs    : Unsigned_64_Array;
      Base    : Unsigned_64;
      Size    : Unsigned_64;
      Phys    : Address;
      Virt    : Address;
      Success : Boolean;
   begin
      Debug.Print ("Arch.CLINT: Configuring CLINT");
      --  Primary: SBI timer extension
      if Probe_Extension (Extension_Timer) then
         Debug.Print ("Arch.CLINT: Using SBI for timer & IPI");
         return;
      end if;
      --  Fallback: MMIO via DTB
      if Node = null then
         Debug.Print ("Arch.CLINT: DTB node not found; disabled");
         return;
      end if;
      --  Parse CLINT reg property
      Regs := Get_Property_Unsigned_64 (Node, "reg");
      Base := Regs (1);
      Size := Regs (2);
      Phys := To_Address (Storage_Offset (Base));
      Virt := Phys;
      --  Map CLINT MMIO
      Map_Range (
        Map            => Kernel_Table,
        Physical_Start => Phys,
        Virtual_Start  => Virt,
        Length         => Storage_Count (Size),
        Permissions    => (Is_User_Accesible => False,
                          Can_Read          => True,
                          Can_Write         => False,
                          Can_Execute       => False,
                          Is_Global         => True),
        Success        => Success
      );
      if not Success then
         Debug.Print ("Arch.CLINT: MMIO map failed; disabled");
         return;
      end if;
      --  Cache base address and offset
      Clint_Base     := Virt;
      Clint_Base_Off := Storage_Offset (To_Integer (Virt));
      Debug.Print ("Arch.CLINT: MMIO mapped at " & Address'Image (Virt)
                   & ", size " & Unsigned_64'Image (Size));
   end Set_CLINT_Configuration;

   ----------------------------------------------------------------------------
   --  Hart initialization: clear MSIP
   ----------------------------------------------------------------------------
   procedure Initialize_Hart (Hart_Id : Unsigned_64) is
      Off : Storage_Offset;
   begin
      pragma Assert (Hart_Id < Core_Count);
      Debug.Print ("Arch.CLINT: Initializing hart " & Unsigned_64'Image (Hart_Id));
      if Probe_Extension (Extension_IPI) then
         Debug.Print ("Arch.CLINT: SBI handles MSIP clear");
         return;
      end if;
      --  Clear MSIP via MMIO
      Off := Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Hart_Id));
      declare
         MSIP : Unsigned_32 with Address => To_Address (Off);
      begin
         MSIP := 0;
         Debug.Print ("Arch.CLINT: MSIP cleared for hart " & Unsigned_64'Image (Hart_Id));
      end;
   end Initialize_Hart;

   ----------------------------------------------------------------------------
   --  Software IPI: send or clear
   ----------------------------------------------------------------------------
   procedure Send_Software_Interrupt (Target_Hart : Unsigned_64) is
      Mask : constant Unsigned_64 := Interfaces.Shift_Left (1_64,
                                 Integer (Target_Hart));
      Off  : Storage_Offset;
   begin
      pragma Assert (Target_Hart < Core_Count);
      if Probe_Extension (Extension_IPI) then
         Debug.Print ("Arch.CLINT: Sending SBI IPI to hart " & Unsigned_64'Image (Target_Hart));
         Send_Ipi (Mask'Address);
         return;
      end if;
      Off := Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Target_Hart));
      declare
         MSIP : Unsigned_32 with Address => To_Address (Off);
      begin
         MSIP := 1;
         Debug.Print ("Arch.CLINT: MSIP set for hart " & Unsigned_64'Image (Target_Hart));
      end;
   end Send_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_Id : Unsigned_64) is
      Off : Storage_Offset;
   begin
      pragma Assert (Hart_Id < Core_Count);
      if Probe_Extension (Extension_IPI) then
         return;
      end if;
      Off := Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Hart_Id));
      declare
         MSIP : Unsigned_32 with Address => To_Address (Off);
      begin
         MSIP := 0;
         Debug.Print ("Arch.CLINT: MSIP cleared for hart " & Unsigned_64'Image (Hart_Id));
      end;
   end Clear_Software_Interrupt;

   procedure Send_Fence_Ipi (Target_Hart : Unsigned_64) is
      Mask : constant Unsigned_64 := Interfaces.Shift_Left (1_64,
                                 Integer (Target_Hart));
   begin
      pragma Assert (Target_Hart < Core_Count);
      if Probe_Extension (Extension_FENCE_IPI) then
         Debug.Print ("Arch.CLINT: Sending SBI fence-IPI to hart " & Unsigned_64'Image (Target_Hart));
         Remote_Fence (Mask'Address);
      else
         Debug.Print ("Arch.CLINT: Fallback to software IPI for fence");
         Send_Software_Interrupt (Target_Hart);
      end if;
   end Send_Fence_Ipi;

   ----------------------------------------------------------------------------
   --  Timer: set MTIMECMP or SBI
   ----------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64) is
      Off     : Storage_Offset;
      HartInt : Integer := Integer (Read_Hart_ID);
   begin
      pragma Assert (HartInt < Integer (Core_Count));
      if Probe_Extension (Extension_Timer) then
         Debug.Print ("Arch.CLINT: Setting SBI timer to " & Unsigned_64'Image (Next_Time));
         Arch.SBI.Set_Timer (Next_Time);
         return;
      end if;
      Off := Clint_Base_Off + MTIMECMP_Base + MTIMECMP_Step * Storage_Offset (HartInt);
      declare
         CMP : Unsigned_64 with Address => To_Address (Off);
      begin
         CMP := Next_Time;
         Debug.Print ("Arch.CLINT: MTIMECMP set to " & Unsigned_64'Image (Next_Time));
      end;
   end Set_Timer;

   procedure Clear_Timer_Interrupt (Hart_Id : Unsigned_64) is
   begin
      pragma Assert (Hart_Id < Core_Count);
      Debug.Print ("Arch.CLINT: Clear_Timer_Interrupt no-op for hart " & Unsigned_64'Image (Hart_Id));
   end Clear_Timer_Interrupt;

   procedure Disable_Timer_Interrupt is
   begin
      Debug.Print ("Arch.CLINT: Disabling timer interrupt (STIE clear)");
      Asm ("li t0, 0x80");
      Asm ("csrc sstatus, t0");
   end Disable_Timer_Interrupt;

   procedure Enable_Timer_Interrupt is
   begin
      Debug.Print ("Arch.CLINT: Enabling timer interrupt (STIE set)");
      Asm ("li t0, 0x80");
      Asm ("csrs sstatus, t0");
   end Enable_Timer_Interrupt;

end Arch.CLINT;