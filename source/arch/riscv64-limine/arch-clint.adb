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

with Arch.CPU;                use Arch.CPU;
with Arch.DTB;                use Arch.DTB;
with Arch.SBI;                use Arch.SBI;
with Arch.MMU;                use Arch.MMU;
with Arch.Debug;              use Arch.Debug;
with Lib.Panic;               use Lib.Panic;
with System.Machine_Code;     use System.Machine_Code;

package body Arch.CLINT with SPARK_Mode => Off is

   ----------------------------------------------------------------------------
   --  CLINT MMIO register offsets (per RISC‑V spec)
   ----------------------------------------------------------------------------
   MSIP_Base      : constant Storage_Offset := 0;
   MTIMECMP_Base  : constant Storage_Offset := 16#4000#;
   MTIMECMP_Step  : constant Storage_Offset := 8;
   MTIME_Base     : constant Storage_Offset := 16#BFF8#;

   ----------------------------------------------------------------------------
   --  CLINT state
   ----------------------------------------------------------------------------
   Clint_Base         : Address        := Null_Address;
   Clint_Base_Off     : Storage_Offset := 0;

   ----------------------------------------------------------------------------
   --  Helper: get an MMIO register’s virtual address
   ----------------------------------------------------------------------------
   function Reg_Addr (Off : Storage_Offset) return Address is
   begin
      return To_Address
        (Integer_Address (Clint_Base_Off + Off));
   end Reg_Addr;

   ----------------------------------------------------------------------------
   --  CLINT availability: prefer SBI, fallback to MMIO
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
      -- A DTB will return 3 registers to configure the CLINT:
      -- Base address, size of the memory region, and additional properties.
      subtype Constrained_Unsigned_64_Array is Unsigned_64_Array (1 .. 3);
      Regs    : Constrained_Unsigned_64_Array;
      Base    : Unsigned_64;
      Size    : Unsigned_64;
      Phys    : Address;
      Virt    : Address;
      Success : Boolean;
   begin
      Debug.Print ("Arch.CLINT: Configuring CLINT");
      if Probe_Extension (Extension_Timer) then
         Debug.Print ("Arch.CLINT: Using SBI for timer & IPI");
         return;
      end if;

      if Node = null then
         Debug.Print ("Arch.CLINT: DTB node not found; CLINT disabled");
         return;
      end if;

      declare
         Regs : Unsigned_64_Array (1 .. 3);
      begin
         Regs := Get_Property_Unsigned_64 (Node, "reg");
         Base := Regs (1);
         Size := Regs (2);
         Phys := To_Address (Integer_Address (Base));
         Virt := Phys;
      end;

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
        Success        => Success,
        Caching        => Write_Back
      );
      if not Success then
         Debug.Print ("Arch.CLINT: MMIO map failed; CLINT disabled");
         return;
      end if;

      Clint_Base     := Virt;
      Clint_Base_Off := Storage_Offset (To_Integer (Virt));
      Debug.Print ("Arch.CLINT: MMIO mapped at " & Address'Image (Clint_Base));

      --  Discover timebase-frequency (Hz), if present
      begin
         declare
            FreqArr : Unsigned_64_Array :=
               Get_Property_Unsigned_64 (Node, "timebase-frequency");
         begin
            Timebase_Frequency := FreqArr (1);
            Debug.Print ("Arch.CLINT: timebase-frequency="
                         & Unsigned_64'Image (Timebase_Frequency));
         end;
      exception
         when others =>
            Debug.Print ("Arch.CLINT: timebase-frequency missing; default="
                         & Unsigned_64'Image (Timebase_Frequency));
      end;
   end Set_CLINT_Configuration;

   ----------------------------------------------------------------------------
   --  Hart-specific initialization: clear MSIP
   ----------------------------------------------------------------------------
   procedure Initialize_Hart (Hart_Id : Unsigned_64) is
      Off : Storage_Offset := 
        Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Hart_Id));
   begin
      pragma Assert (Hart_Id < Unsigned_64 (Core_Count));
      Debug.Print ("Arch.CLINT: Initializing hart "
                   & Unsigned_64'Image (Hart_Id));

      if Probe_Extension (Extension_Ipi) then
         Debug.Print ("Arch.CLINT: SBI handles MSIP clear");
         return;
      end if;

      declare
         MSIP : Unsigned_32 with Address =>
                  To_Address (Integer_Address (Off));
      begin
         MSIP := 0;
         Debug.Print ("Arch.CLINT: MSIP cleared for hart "
                      & Unsigned_64'Image (Hart_Id));
      end;
   end Initialize_Hart;

   ----------------------------------------------------------------------------
   --  Software IPIs
   ----------------------------------------------------------------------------
   procedure Send_Software_Interrupt (Target_Hart : Unsigned_64) is
      Mask : constant Unsigned_64 :=
        Shift_Left (Unsigned_64 (1), Integer (Target_Hart));
      Off  : Storage_Offset :=
        Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Target_Hart));
   begin
      pragma Assert (Target_Hart < Unsigned_64 (Core_Count));
      if Probe_Extension (Extension_Ipi) then
         Debug.Print ("Arch.CLINT: Sending SBI IPI to hart "
                      & Unsigned_64'Image (Target_Hart));
         Send_Ipi (Mask'Address);
      else
         declare
            MSIP : Unsigned_32 with Address =>
                     To_Address (Integer_Address (Off));
         begin
            MSIP := 1;
            Debug.Print ("Arch.CLINT: MSIP set for hart "
                         & Unsigned_64'Image (Target_Hart));
         end;
      end if;
   end Send_Software_Interrupt;

   procedure Clear_Software_Interrupt (Hart_Id : Unsigned_64) is
      Off : Storage_Offset :=
        Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Hart_Id));
   begin
      pragma Assert (Hart_Id < Unsigned_64 (Core_Count));
      if Probe_Extension (Extension_Ipi) then
         return;
      end if;
      declare
         MSIP : Unsigned_32 with Address =>
                  To_Address (Integer_Address (Off));
      begin
         MSIP := 0;
         Debug.Print ("Arch.CLINT: MSIP cleared for hart "
                      & Unsigned_64'Image (Hart_Id));
      end;
   end Clear_Software_Interrupt;

   procedure Send_Fence_Ipi (Target_Hart : Unsigned_64) is
      Mask : constant Unsigned_64 :=
        Shift_Left (Unsigned_64 (1), Integer (Target_Hart));
   begin
      pragma Assert (Target_Hart < Unsigned_64 (Core_Count));
      if Probe_Extension (Extension_Fence_Ipi) then
         Debug.Print ("Arch.CLINT: Sending SBI fence‑IPI to hart "
                      & Unsigned_64'Image (Target_Hart));
         Remote_Fence (Mask'Address);
      else
         Debug.Print ("Arch.CLINT: Fallback to software IPI for fence");
         Send_Software_Interrupt (Target_Hart);
      end if;
   end Send_Fence_Ipi;

   ----------------------------------------------------------------------------
   --  Timer interrupts
   ----------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64) is
      Off     : Storage_Offset;
      HartInt : Integer := Integer (Read_Hart_ID);
   begin
      if Probe_Extension (Extension_Timer) then
         Debug.Print ("Arch.CLINT: Setting SBI timer to "
                      & Unsigned_64'Image (Next_Time));
         Set_Timer (Next_Time);
      else
         Off := Clint_Base_Off
                + MTIMECMP_Base
                + MTIMECMP_Step * Storage_Offset (HartInt);
         declare
            CMP : Unsigned_64 with Address =>
                    To_Address (Integer_Address (Off));
         begin
            CMP := Next_Time;
            Debug.Print ("Arch.CLINT: MTIMECMP set to "
                         & Unsigned_64'Image (Next_Time));
         end;
      end if;
   end Set_Timer;

   procedure Clear_Timer_Interrupt (Hart_Id : Unsigned_64) is
   begin
      Debug.Print ("Arch.CLINT: Clear_Timer_Interrupt no‑op for hart "
                   & Unsigned_64'Image (Hart_Id));
   end Clear_Timer_Interrupt;

   procedure Disable_Timer_Interrupt is
   begin
      Debug.Print ("Arch.CLINT: Disabling timer interrupt (STIE clear)");
      Asm ("li t0, 0x80", Volatile => True);
      Asm ("csrc sstatus, t0", Volatile => True);
   end Disable_Timer_Interrupt;

   procedure Enable_Timer_Interrupt is
   begin
      Debug.Print ("Arch.CLINT: Enabling timer interrupt (STIE set)");
      Asm ("li t0, 0x80", Volatile => True);
      Asm ("csrs sstatus, t0", Volatile => True);
   end Enable_Timer_Interrupt;

   ----------------------------------------------------------------------------
   --  Read current time (for Reschedule_In and other uses)
   ----------------------------------------------------------------------------
   function Read_Timer return Unsigned_64 is
   begin
      if Probe_Extension (Extension_Timer) then
         return Get_Time;
      else
         declare
            MTIME : Unsigned_64 with Address =>
                     To_Address (Integer_Address (MTIME_Base));
         begin
            return MTIME;
         end;
      end if;
   end Read_Timer;

end Arch.CLINT;