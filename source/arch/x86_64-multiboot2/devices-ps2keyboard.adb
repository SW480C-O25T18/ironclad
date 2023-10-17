--  devices-ps2keyboard.adb: PS2 keyboard driver.
--  Copyright (C) 2023 streaksu
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

with Arch.IDT;
with Arch.APIC;
with Arch.CPU;
with Arch.Snippets;
with Scheduler;

package body Devices.PS2Keyboard with SPARK_Mode => Off is
   type Scancode_Arr is array (1 .. 10) of Unsigned_8;

   --  Globals to communicate with the interrupt routine.
   Is_Reading     : Boolean      with Volatile;
   Scancode_Count : Natural      with Volatile;
   Scancodes      : Scancode_Arr with Volatile;

   function Init return Boolean is
      BSP_LAPIC : constant Unsigned_32 := Arch.CPU.Core_Locals (1).LAPIC_ID;
      Index     : Arch.IDT.IRQ_Index;
      Data      : Unsigned_8;
      Success   : Boolean;
   begin
      --  Set the interrupt up, which is always the 34 (we are 1 based).
      if not Arch.IDT.Load_ISR (Keyboard_Handler'Address, Index) then
         return False;
      end if;
      if not Arch.APIC.IOAPIC_Set_Redirect (BSP_LAPIC, 34, Index, True) then
         return False;
      end if;

      --  Take the chance for initializing the PS2 controller.
      --  Disable primary and secondary PS/2 ports
      Arch.Snippets.Port_Out (16#64#, 16#AD#);
      Arch.Snippets.Port_Out (16#64#, 16#A7#);

      --  Read from port 0x60 to flush the PS/2 controller buffer.
      while (Arch.Snippets.Port_In (16#64#) and 1) /= 0 loop
         Data := Arch.Snippets.Port_In (16#60#);
      end loop;

      --  Enable keyboard interrupt and keyboard scancode translation.
      Data := Read_PS2_Config;
      Data := Data or Shift_Left (1, 0) or Shift_Left (1, 6);

      --  Enable mouse interrupt if any
      if (Data and Shift_Left (1, 5)) /= 0 then
         Data := Data or Shift_Left (1, 1);
      end if;

      --  Write changes, enable keyboard port, and enable mouse port if any.
      Write_PS2_Config (Data);
      Write_PS2 (16#64#, 16#AE#);
      if (Data and Shift_Left (1, 5)) /= 0 then
         Write_PS2 (16#64#, 16#A8#);
      end if;

      Register
         ((Data        => System.Null_Address,
           ID          => (others => 0),
           Is_Block    => False,
           Block_Size  => 4096,
           Block_Count => 0,
           Sync        => null,
           Sync_Range  => null,
           Read        => Read'Access,
           Write       => null,
           IO_Control  => null,
           Mmap        => null,
           Poll        => null), "ps2keyboard", Success);
      return Success;
   end Init;

   function Read_PS2 return Unsigned_8 is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 1) = 0 loop
         Scheduler.Yield_If_Able;
      end loop;
      return Arch.Snippets.Port_In (16#60#);
   end Read_PS2;

   procedure Write_PS2 (Port : Unsigned_16; Value : Unsigned_8) is
   begin
      while (Arch.Snippets.Port_In (16#64#) and 2) /= 0 loop
         Scheduler.Yield_If_Able;
      end loop;
      Arch.Snippets.Port_Out (Port, Value);
   end Write_PS2;

   function Read_PS2_Config return Unsigned_8 is
   begin
      Write_PS2 (16#64#, 16#20#);
      return Read_PS2;
   end Read_PS2_Config;

   procedure Write_PS2_Config (Value : Unsigned_8) is
   begin
      Write_PS2 (16#64#, 16#60#);
      Write_PS2 (16#60#, Value);
   end Write_PS2_Config;

   procedure Read
      (Key       : System.Address;
       Offset    : Unsigned_64;
       Data      : out Operation_Data;
       Ret_Count : out Natural;
       Success   : out Boolean)
   is
      pragma Unreferenced (Key);
      pragma Unreferenced (Offset);
      Copied : Natural;
   begin
      Scancode_Count := 0;
      Is_Reading     := True;
      while Is_Reading loop
         Scheduler.Yield_If_Able;
      end loop;
      Is_Reading := False;

      if Scancode_Count > Data'Length then
         Copied := Data'Length;
      else
         Copied := Scancode_Count;
      end if;

      for I in 1 .. Copied loop
         Data (Data'First + I - 1) := Scancodes (I);
      end loop;

      Success   := True;
      Ret_Count := Scancode_Count;
   end Read;

   procedure Keyboard_Handler is
      Input : constant Unsigned_8 := Arch.Snippets.Port_In (16#60#);
   begin
      if Is_Reading and Scancode_Count < Scancodes'Length then
         Scancodes (Scancodes'First + Scancode_Count) := Input;
         Scancode_Count := Scancode_Count + 1;
         if Input /= 16#E0# then
            Is_Reading := False;
         end if;
      end if;

      Arch.APIC.LAPIC_EOI;
   end Keyboard_Handler;
end Devices.PS2Keyboard;
