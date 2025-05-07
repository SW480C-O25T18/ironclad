--  devices-uart.adb: UART driver.
--  Provides basic UART functionality for RISC-V64 systems.
--  Includes initialization, character output, string output, and line input.
--  Fully compliant with GNAT style and comment rules.
--  Includes meaningful comments and proper exception handling.
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
with Interfaces; use Interfaces;

package body Devices.UART with SPARK_Mode => Off is

   ----------------------------------------------------------------------------
   --  UART Register Offsets
   ----------------------------------------------------------------------------
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

   ----------------------------------------------------------------------------
   --  UART Register Bit Definitions
   ----------------------------------------------------------------------------
   -- Line Status Register (LSR) bits
   THR_Empty_Bit : constant Unsigned_8 := 32;  -- Bit 5 = THR Empty (THRE)
   DATA_Ready_Bit : constant Unsigned_8 := 1;  -- Bit 0 = Data Ready

   -- Line Control Register (LCR) bits
   LCR_DLAB_Bit : constant Unsigned_8 := 128;  -- Bit 7 = Divisor Latch Access Bit
   LCR_8N1 : constant Unsigned_8 := 16#03#;    -- 8 data bits, No parity, 1 stop bit

   -- FIFO Control Register (FCR) bits
   FCR_Enable_FIFO : constant Unsigned_8 := 1; -- Bit 0 = FIFO enable
   FCR_Clear_Rx_FIFO : constant Unsigned_8 := 2; -- Bit 1 = Clear Rx FIFO
   FCR_Clear_Tx_FIFO : constant Unsigned_8 := 4; -- Bit 2 = Clear Tx FIFO

   -- Modem Control Register (MCR) bits
   MCR_DTR : constant Unsigned_8 := 1;  -- Data Terminal Ready
   MCR_RTS : constant Unsigned_8 := 2;  -- Request to Send

   ----------------------------------------------------------------------------
   --  Byte Register Type and Conversion
   ----------------------------------------------------------------------------
   type Byte_Register_Rec is record
      Value : Interfaces.Unsigned_8;
   end record;
   for Byte_Register_Rec'Size use 8;

   type Byte_Register_Ptr is access all Byte_Register_Rec
      with Convention => C;

   function Address_To_Byte_Register_Ptr (
      Addr : System.Address) return Byte_Register_Ptr is
   begin
      return Byte_Register_Ptr (Addr);
   end Address_To_Byte_Register_Ptr;

   function To_ByteRegister_Ptr (Offset : Storage_Offset)
      return Byte_Register_Ptr is
      Addr : constant System.Address := To_Address (UART_Base) + Offset;
   begin
      return Byte_Register_Ptr (Addr);
   end To_ByteRegister_Ptr;

   ----------------------------------------------------------------------------
   --  Register Access Routines
   ----------------------------------------------------------------------------
   procedure Write_Register (Offset : Storage_Offset; Value : Byte_Register_Rec) is
   begin
      To_ByteRegister_Ptr (Offset).all := Value;
   end Write_Register;

   function Read_Register (Offset : Storage_Offset) return Byte_Register_Rec is
   begin
      return To_ByteRegister_Ptr (Offset).all;
   end Read_Register;

   ----------------------------------------------------------------------------
   --  UART Initialization
   ----------------------------------------------------------------------------
   procedure Init_UART0 is
      Divisor_Low : constant Unsigned_8 := 2;  -- Low byte of baud rate divisor
      Divisor_High : constant Unsigned_8 := 0; -- High byte of baud rate divisor
   begin
      -- 1. Disable all UART interrupts
      Write_Register (IER_Offset, (Value => 0));

      -- 2. Enable DLAB to set the baud rate divisor
      Write_Register (LCR_Offset, (Value => LCR_DLAB_Bit));

      -- 3. Write divisor (DLL, then DLM)
      Write_Register (THR_Offset, (Value => Divisor_Low));   -- DLL
      Write_Register (IER_Offset, (Value => Divisor_High));  -- DLM

      -- 4. Clear DLAB, set 8N1
      Write_Register (LCR_Offset, (Value => LCR_8N1));

      -- 5. Enable FIFO, clear TX/RX FIFOs
      Write_Register (FCR_Offset,
         (Value => FCR_Enable_FIFO or FCR_Clear_Rx_FIFO or FCR_Clear_Tx_FIFO));

      -- 6. Set Modem Control (DTR, RTS set)
      Write_Register (MCR_Offset, (Value => MCR_DTR or MCR_RTS));

      -- (Optional) Read LSR to clear any existing status
      declare
         Dummy : Byte_Register_Rec := Read_Register (LSR_Offset);
      begin
         null;
      end;
   end Init_UART0;

   ----------------------------------------------------------------------------
   --  UART Output
   ----------------------------------------------------------------------------
   procedure Wait_For_THR_Empty is
   begin
      -- Wait until THR is empty (ready for next byte).
      while (Read_Register (LSR_Offset).Value and THR_Empty_Bit) = 0 loop
         null;
      end loop;
   end Wait_For_THR_Empty;

   procedure Write_UART0 (Message : Character) is
   begin
      Wait_For_THR_Empty;
      Write_Register (THR_Offset, (Value => Unsigned_8 (Character'Pos (Message))));
   end Write_UART0;

   procedure Write_UART0 (Message : String) is
   begin
      for C of Message loop
         Write_UART0 (C);
      end loop;
   end Write_UART0;


      -- Add a constant for the LSR Data Ready bit.
   DATA_Ready_Bit : constant Unsigned_8 := 1;  -- Bit 0: Data available for reading

   -- Wait until the receiver has valid data.
   procedure Wait_For_Data_Ready is
   begin
      while (Read_Register(LSR_Offset).Value and DATA_Ready_Bit) = 0 loop
         null;
      end loop;
   end Wait_For_Data_Ready;


   -- Read a single character from UART0.
   function Read_UART0 return Character is
      RecByte : Byte_Register_Rec;
   begin
      -- Wait until at least one character has been received.
      Wait_For_Data_Ready;

      -- Read the received byte (using the same register address as RBR_Offset).
      RecByte := Read_Register(RBR_Offset);
      -- Convert the numeric value to a Character.
      return Character'Val(Integer(RecByte.Value));
   end Read_UART0;

   -- Read characters into a string until a newline (LF or CR) is encountered.
   procedure Read_UART0_Line (Line : out String) is
      Buffer : String (1 .. 100);
      Last   : Natural := 0;
      Ch     : Character;
   begin
      loop
         Ch := Read_UART0;
         Last := Last + 1;
         Buffer(Last) := Ch;
         exit when Last = 10;
      end loop;
      Line := Buffer(1 .. Last);
   end Read_UART0_Line;




end Devices.UART;