--  arch-entrypoint.adb: Limine plops us here.
--  Copyright (C) 2024 streaksu
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

with System; use System;
with System.Storage_Elements;
with Arch.Debug;
with Devices.UART;
with Arch.Limine;
with Lib.Messages;      use Lib.Messages;
with Memory.Physical;
with Arch.MMU;
with Lib.Panic;
with Arch.DTB;          use Arch.DTB;
with Arch.CPU;
with Main;
with Arch.Interrupts;
with Arch.CLINT;
with Arch.PLIC;
with Arch.ACPI;
with Arch.Clocks;
with Ada.Unchecked_Conversion;
with Arch.Snippets;     use Arch.Snippets;

package body Arch.Entrypoint is

   --  Convert a 64-bit unsigned value into an Integer_Address
   function U64_To_Int_Addr is new Ada.Unchecked_Conversion
     ( Source => Unsigned_64,
       Target => System.Storage_Elements.Integer_Address
     );

   --  Convert a System.Address to Unsigned_64
   function Address_To_U64 is new Ada.Unchecked_Conversion
     ( Source => System.Address,
       Target => Unsigned_64
     );

   --  Convert Unsigned_64 to String (fixed-length)
   function Unsigned_To_String (Value : Unsigned_64)
      return Context_String
   is
      Buffer        : Context_String := (others => ' ');
      I             : Integer        := Buffer'Last;
      N             : Unsigned_64    := Value;
      Overflow_Str  : constant Context_String :=
        "Overflow            ";  -- length 20
      Unknown_Str   : constant Context_String :=
        "Unknown Error       ";  -- length 20
   begin
      if N = 0 then
         Buffer (I) := '0';
      else
         while N > 0 loop
            Buffer (I) := Character'Val
              ( Character'Pos ('0') + Integer (N mod 10) );
            N := N / 10;
            I := I - 1;
         end loop;
      end if;
      return Buffer;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("[Error] Constraint error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Constraint error in Unsigned_To_String");
         return Overflow_Str;
      when Program_Error =>
         Arch.Debug.Print ("[Error] Program error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Program error in Unsigned_To_String");
         return Unknown_Str;
      when Storage_Error =>
         Arch.Debug.Print ("[Error] Storage error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Storage error in Unsigned_To_String");
         return Unknown_Str;
      when others =>
         Arch.Debug.Print ("[Error] Unknown error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Unknown error in Unsigned_To_String");
         return Unknown_Str;
   end Unsigned_To_String;

   ----------------------------------------------------------------------
   --  Centralized exception handler for better debugging
   ----------------------------------------------------------------------
   procedure Handle_Exception (Context : Context_String) is
   begin
      Arch.Debug.Print (
         "[Error] Exception occurred during " & Context
      );
      Lib.Panic.Hard_Panic (
         "Exception occurred during " & Context
      );
   exception
      when Constraint_Error =>
         Arch.Debug.Print (
            "[Error] Constraint error during " & Context
         );
         Snippets.HCF;
      when Program_Error =>
         Arch.Debug.Print (
            "[Error] Program error during " & Context
         );
         Snippets.HCF;
      when Storage_Error =>
         Arch.Debug.Print (
            "[Error] Storage error during " & Context
         );
         Snippets.HCF;
      when others =>
         Arch.Debug.Print (
            "[Error] Unknown error during " & Context
         );
         Snippets.HCF;
   end Handle_Exception;

   ----------------------------------------------------------------------
   --  Bootstrap_Main
   ----------------------------------------------------------------------
   procedure Bootstrap_Main is
      Info       : Boot_Information renames Limine.Global_Info;
      Addr       : System.Address;
      Num_Harts  : Unsigned_64;
      CLINT_Node : DTB_Node_Access;
      PLIC_Node  : DTB_Node_Access;
   begin
      -- UART0 Initialization
      begin
         Arch.Debug.Print ("[Stage 1] Initializing UART0...");
         if not Devices.UART.Init_UART0 then
            Lib.Panic.Hard_Panic ("UART0 initialization failed");
         end if;
         Arch.Debug.Print ("[Stage 1] UART0 initialized successfully");
      exception
         when others => Handle_Exception ("UART0 initialization");
      end;

      -- Limine Protocol Translation
      begin
         Arch.Debug.Print ("[Stage 2] Translating Limine protocol...");
         Limine.Translate_Proto;
         Arch.Debug.Print ("[Stage 2] Limine translated successfully");
      exception
         when others => Handle_Exception ("Limine protocol translation");
      end;

      -- DTB Initialization
      begin
         Arch.Debug.Print ("[Stage 3] Initializing DTB...");
         if not Arch.DTB.Init then
            Lib.Panic.Hard_Panic ("No DTB was found!");
         end if;
         Arch.Debug.Print ("[Stage 3] DTB initialized successfully");
         Arch.Debug.Print ("[Stage 3] Printing DTB contents:");
         Print_DTB_Node (Arch.DTB.Root_Node);
      exception
         when others => Handle_Exception ("DTB initialization");
      end;

      -- Allocators & MMU Initialization
      begin
         Arch.Debug.Print ("[Stage 4] Initializing allocators and MMU...");
         Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
         Arch.Debug.Print ("[Stage 4] Physical allocator initialized");
         if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
            Lib.Panic.Hard_Panic ("The VMM could not be initialized");
         end if;
         Arch.Debug.Print ("[Stage 4] MMU initialized successfully");
      exception
         when others => Handle_Exception ("Allocator or MMU initialization");
      end;

      -- Logging Initialization
      begin
         Arch.Debug.Print ("[Stage 5] Enabling logging...");
         Lib.Messages.Enable_Logging;
         Arch.Debug.Print ("[Stage 5] Logging enabled");
      exception
         when others => Handle_Exception ("Logging initialization");
      end;

      -- Memory Map Dump
      begin
         Arch.Debug.Print ("[Stage 6] Dumping physical memory map:");
         for E of Info.Memmap (1 .. Info.Memmap_Len) loop
            Addr := E.Start + E.Length;
            Arch.Debug.Print (
              "[" & Unsigned_To_String (Address_To_U64 (E.Start))
              & " - " & Unsigned_To_String (Address_To_U64 (Addr))
              & "] " & Boot_Memory_Type'Image (E.MemType)
            );
         end loop;
         Arch.Debug.Print ("[Stage 6] Memory map dump complete");
      exception
         when others => Handle_Exception ("Memory map dump");
      end;

      -- CPU Initialization
      begin
         Arch.Debug.Print ("[Stage 7] Initializing CPU cores...");
         Arch.CPU.Init_Cores;
         Num_Harts := Unsigned_64 (Arch.CPU.Core_Count);
         Arch.Debug.Print (
            "[Stage 7] CPU cores initialized: " &
            Unsigned_To_String (Num_Harts)
         );
      exception
         when others => Handle_Exception ("CPU core initialization");
      end;

      -- CLINT Configuration
      begin
         Arch.Debug.Print ("[Stage 8] Configuring CLINT...");
         CLINT_Node := Find_Node_By_Compatible ("riscv,clint");
         if CLINT_Node /= null then
            Arch.CLINT.Set_CLINT_Configuration;
            Arch.Debug.Print ("[Stage 8] CLINT configured successfully");
         else
            Arch.Debug.Print ("[Stage 8] CLINT not found; using defaults");
         end if;
      exception
         when others => Handle_Exception ("CLINT configuration");
      end;

      -- PLIC Configuration
      begin
         Arch.Debug.Print ("[Stage 9] Configuring PLIC...");
         PLIC_Node := Find_Node_By_Compatible ("riscv,plic");
         if PLIC_Node /= null then
            Arch.PLIC.Set_PLIC_Configuration;
            Arch.Debug.Print ("[Stage 9] PLIC configured successfully");
         else
            Arch.Debug.Print ("[Stage 9] PLIC not found; using defaults");
         end if;
      exception
         when others => Handle_Exception ("PLIC configuration");
      end;

      -- Trap Handling Configuration
      begin
         Arch.Debug.Print ("[Stage 10] Setting up trap handling...");
         Arch.CPU.Set_Trap_Vector;
         Arch.Debug.Print ("[Stage 10] Trap handling initialized");
      exception
         when others => Handle_Exception ("Trap handling initialization");
      end;

      -- Interrupts Initialization
      begin
         Arch.Debug.Print ("[Stage 11] Initializing interrupts...");
         Arch.Interrupts.Initialize;
         Arch.Debug.Print ("[Stage 11] Interrupts initialized");
      exception
         when others => Handle_Exception ("Interrupt initialization");
      end;

      -- ACPI Initialization
      begin
         Arch.Debug.Print ("[Stage 12] Initializing ACPI...");
         declare
            Success : Boolean;
         begin
            Arch.ACPI.Initialize (Success);
            if Success then
               Arch.Debug.Print ("[Stage 12] ACPI initialized successfully");
            else
               Arch.Debug.Print ("[Stage 12] ACPI not supported.");
            end if;
         end;
      exception
         when others => Handle_Exception ("ACPI initialization");
      end;

      -- Clock Sources Initialization
      begin
         Arch.Debug.Print ("[Stage 13] Initializing clock sources...");
         Arch.Clocks.Initialize_Sources;
         Arch.Debug.Print ("[Stage 13] Clock sources initialized");
      exception
         when others => Handle_Exception ("Clock sources initialization");
      end;

      -- Command Line & Main Kernel
      begin
         Arch.Debug.Print ("[Stage 14] Copying command line...");
         Arch.Cmdline_Len := Info.Cmdline_Len;
         Arch.Cmdline
           (1 .. Info.Cmdline_Len) :=
           Info.Cmdline
             (1 .. Info.Cmdline_Len);
         Arch.Debug.Print ("[Stage 14] Command line copied");
         Arch.Debug.Print ("[Stage 14] Jumping to main kernel...");
         Main;
      exception
         when others => Handle_Exception ("Command line setup or main execution");
      end;
   end Bootstrap_Main;

end Arch.Entrypoint;
