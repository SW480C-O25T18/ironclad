--  devices-uart.adb: UART driver.
--  Copyright (C) 2024 streaksu
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
----  devices-uart.adb: UART driver.
--  Copyright (C) 2024 streaksu
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

with System.Storage_Elements; use System.Storage_Elements;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package body Devices.UART with SPARK_Mode => Off is

   THR_Offset : constant Storage_Offset := 0;  -- Transmit Holding Register
   RBR_Offset : constant Storage_Offset := 0;  -- Receive Buffer Register
   IER_Offset : constant Storage_Offset := 1;  -- Interrupt Enable Register
   IIR_Offset : constant Storage_Offset := 2;  -- Interrupt Identification (read)
   FCR_Offset : constant Storage_Offset := 2;  -- FIFO Control (write)
   LCR_Offset : constant Storage_Offset := 3;  -- Line Control
   MCR_Offset : constant Storage_Offset := 4;  -- Modem Control
   LSR_Offset : constant Storage_Offset := 5;  -- Line Status
   MSR_Offset : constant Storage_Offset := 6;  -- Modem Status
   SCR_Offset : constant Storage_Offset := 7;  -- Scratch Register

   -- LSR bits
   THR_Empty_Bit : constant Unsigned_8 := 32;  -- Bit 5 = THR Empty (THRE)

   -- LCR bits
   LCR_DLAB_Bit  : constant Unsigned_8 := 128;  -- Bit 7 = Divisor Latch Access Bit

   -- Typical 8N1: 8 data bits, No parity, 1 stop bit
   LCR_8N1 : constant Unsigned_8 := 16#03#;  

   -- FIFO Control bits
   FCR_Enable_FIFO    : constant Unsigned_8 := 1; -- Bit 0 = FIFO enable
   FCR_Clear_Rx_FIFO  : constant Unsigned_8 := 2; -- Bit 1 = Clear Rx FIFO
   FCR_Clear_Tx_FIFO  : constant Unsigned_8 := 4; -- Bit 2 = Clear Tx FIFO

   -- Modem Control bits
   MCR_DTR : constant Unsigned_8 := 1;  -- DTR
   MCR_RTS : constant Unsigned_8 := 2;  -- RTS

   -- A type for accessing a single byte in memory.
   type Byte_Register_Rec is record
      Value : Interfaces.Unsigned_8;
   end record;
   for Byte_Register_Rec'Size use 8;

   type Byte_Register_Ptr is access all Byte_Register_Rec
   with Convention => C;

   -- Declare an unchecked conversion function.
   function Address_To_Byte_Register_Ptr is new Ada.Unchecked_Conversion (System.Address, Byte_Register_Ptr);

   function To_ByteRegister_Ptr (Offset : Storage_Offset)
      return Byte_Register_Ptr
   is
      Addr : constant System.Address := To_Address (UART_Base) + Offset;
   begin
      return Address_To_Byte_Register_Ptr (Addr);
   end To_ByteRegister_Ptr;

   -- Handy routines for reading/writing bytes at a register offset
   procedure Write_Register (Offset : Storage_Offset; Value : Byte_Register_Rec) is
   begin
      To_ByteRegister_Ptr(Offset).all := Value;
   end Write_Register;

   function Read_Register (Offset : Storage_Offset) return Byte_Register_Rec is
   begin
      return To_ByteRegister_Ptr(Offset).all;
   end Read_Register;



   procedure Init_UART0 is
      Divisor_Low  : constant Unsigned_8 := 2; 
      Divisor_High : constant Unsigned_8 := 0; 
   begin
      -- 1. Disable all UART interrupts
      Write_Register(IER_Offset, (Value => 0));

      -- 2. Enable DLAB to set the baud rate divisor
      Write_Register(LCR_Offset, (Value => LCR_DLAB_Bit));

      -- 3. Write divisor (DLL, then DLM)
      Write_Register(THR_Offset, (Value => Divisor_Low));   -- DLL
      Write_Register(IER_Offset, (Value => Divisor_High));  -- DLM

      -- 4. Clear DLAB, set 8N1
      Write_Register(LCR_Offset, (Value => LCR_8N1));

      -- 5. Enable FIFO, clear TX/RX FIFOs
      Write_Register(FCR_Offset,
                     (Value => FCR_Enable_FIFO or FCR_Clear_Rx_FIFO or FCR_Clear_Tx_FIFO));

      -- 6. Set Modem Control (DTR, RTS set)
      Write_Register(MCR_Offset, (Value => MCR_DTR or MCR_RTS));

      -- (Optional) Read LSR to clear any existing status
      declare
         Dummy : Byte_Register_Rec := Read_Register(LSR_Offset);
      begin
         null;
      end;
   end Init_UART0;

   procedure Wait_For_THR_Empty is
   begin
      -- Wait until THR is empty (ready for next byte).
      while (Read_Register(LSR_Offset).Value and THR_Empty_Bit) = 0 loop
         null;
      end loop;
   end Wait_For_THR_Empty;

   procedure Write_UART0 (Message : Character) is
   begin
      Wait_For_THR_Empty;
      Write_Register(THR_Offset, (Value => Unsigned_8(Character'Pos(Message))));
   end Write_UART0;

   procedure Write_UART0 (Message : String) is
   begin
      for C of Message loop
         Write_UART0 (C);
      end loop;
   end Write_UART0;
end Devices.UART;
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

package body Devices.UART with SPARK_Mode => Off is
   procedure Init_UART0 is
   begin
      null;
   end Init_UART0;

   procedure Write_UART0 (Message : Character) is
      Data : Unsigned_8 with Import, Address => Message'Address;
   begin
      UART0_TX := Data;
   end Write_UART0;

   procedure Write_UART0 (Message : String) is
   begin
      for C of Message loop
         Write_UART0 (C);
      end loop;
   end Write_UART0;
end Devices.UART;
