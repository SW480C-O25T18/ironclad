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

with Arch.Debug;
with Interfaces;        use Interfaces;
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
with Ada.Unchecked_Conversion;

pragma Warnings (Off);

package body Arch.Entrypoint is

   --  Helper: convert a 64-bit unsigned value into an Integer_Address
   function U64_To_Int_Addr is new Ada.Unchecked_Conversion
     (Source => Unsigned_64,
      Target => System.Storage_Elements.Integer_Address);

   ------------------------------------------------------------------------
   --  Bootstrap_Main
   ------------------------------------------------------------------------
   procedure Bootstrap_Main is
      Info       : Boot_Information renames Limine.Global_Info;
      Addr       : System.Address;
      Num_Harts  : Unsigned_64;
      CLINT_Node : DTB_Node_Access;
      PLIC_Node  : DTB_Node_Access;
   begin
      --  UART0 init
      begin
         if not Devices.UART.Init_UART0 then
            Debug.Print ("Devices.UART.Init_UART0 failed");
         end if;
         Arch.Debug.Print ("Hello from kernel entrypoint");
      exception
         when others =>
            Debug.Print ("Exception occurred during UART0 initialization");
            Lib.Panic.Hard_Panic ("UART0 initialization failed");
      end;

      --  1. Limine → arch
      begin
         Arch.Debug.Print ("Translating Limine protocol");
         Limine.Translate_Proto;
      exception
         when others =>
            Debug.Print (
               "Exception occurred during Limine protocol translation");
            Lib.Panic.Hard_Panic (
               "Limine protocol translation failed");
      end;

      --  2. DTB init
      begin
         Arch.Debug.Print ("Initializing DTB discovery");
         if not Arch.DTB.Init then
            Lib.Panic.Hard_Panic ("No DTB was found!");
         end if;
         Arch.Debug.Print ("DTB initialized successfully");
      exception
         when others =>
            Debug.Print ("Exception occurred during DTB initialization");
            Lib.Panic.Hard_Panic ("DTB initialization failed");
      end;

      --  3. Allocators & MMU
      begin
         Debug.Print ("Initializing allocators and MMU");
         Arch.Debug.Print ("Initializing allocators");
         Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
         Debug.Print ("Physical allocator initialized");
         Debug.Print ("Initializing MMU");
         if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
            Lib.Panic.Hard_Panic ("The VMM could not be initialized");
         end if;
         Debug.Print ("MMU initialized");
      exception
         when others =>
            Debug.Print ("Exception occurred during allocator or MMU initialization");
            Lib.Panic.Hard_Panic ("Allocator or MMU initialization failed");
      end;

      --  4. Logging
      begin
         Debug.Print ("Enabling logging");
         Lib.Messages.Enable_Logging;
         Debug.Print ("Logging enabled");
      exception
         when others =>
            Debug.Print ("Exception occurred during logging initialization");
            Lib.Panic.Hard_Panic ("Logging initialization failed");
      end;

      --  5. Dump memory map
      begin
         Arch.Debug.Print ("Physical memory map:");
         for E of Info.Memmap (1 .. Info.Memmap_Len) loop
            Addr := E.Start + E.Length;
            Arch.Debug.Print (
              "[" & E.Start'Image & " - " & Addr'Image & "] "
              & Boot_Memory_Type'Image (E.MemType));
         end loop;
      exception
         when others =>
            Debug.Print ("Exception occurred while dumping memory map");
            Lib.Panic.Hard_Panic ("Memory map dump failed");
      end;

      --  6. CPU cores
      begin
         Arch.Debug.Print ("Initializing CPU cores");
         Arch.CPU.Init_Cores;
         Num_Harts := Unsigned_64 (Arch.CPU.Core_Count);
         Arch.Debug.Print (
           "CPU cores initialized: " & Unsigned_64'Image (Num_Harts));
      exception
         when others =>
            Debug.Print (
               "Exception occurred during CPU core initialization");
            Lib.Panic.Hard_Panic (
               "CPU core initialization failed");
      end;

      --  7. CLINT config
      begin
         Arch.Debug.Print ("Search for CLINT node in DTB");
         CLINT_Node := Find_Node_By_Compatible ("riscv,clint");
         if CLINT_Node = null then
            CLINT_Node := Find_Node_By_Compatible (
               "riscv,interrupt-controller");
            Arch.Debug.Print (
               "CLINT_Node (fallback): " & CLINT_Node'Image);
         end if;
         if CLINT_Node /= null then
            Print_DTB_Node (CLINT_Node);
            declare
               CLINT_Reg : Unsigned_64_Array :=
                 Get_Property_Unsigned_64 (CLINT_Node, "reg");
            begin
               if CLINT_Reg'Length >= 4 then
                  Arch.CLINT.Set_CLINT_Configuration (
                    Base_Address => System.Storage_Elements.To_Address (
                     U64_To_Int_Addr (CLINT_Reg (1))),
                    MSIP_Offset     => CLINT_Reg (2),
                    MTime_Offset    => CLINT_Reg (3),
                    MTimecmp_Offset => CLINT_Reg (4),
                    Enabled         => True);
                  Arch.Debug.Print ("CLINT configured from DTB.");
               else
                  Arch.Debug.Print (
                    "CLINT DTB info incomplete; using defaults.");
                  Arch.CLINT.Set_CLINT_Configuration;
               end if;
            end;
         else
            Arch.Debug.Print (
              "CLINT node not found; using defaults.");
            Arch.CLINT.Set_CLINT_Configuration;
         end if;
      exception
         when others =>
            Debug.Print ("Exception occurred during CLINT configuration");
            Lib.Panic.Hard_Panic ("CLINT configuration failed");
      end;

      --  8. PLIC config
      begin
         Arch.Debug.Print ("Search for PLIC node in DTB");
         PLIC_Node := Find_Node_By_Compatible ("riscv,plic");
         if PLIC_Node = null then
            PLIC_Node := Find_Node_By_Compatible ("riscv,interrupt-controller");
            Arch.Debug.Print ("PLIC_Node (fallback): " & PLIC_Node'Image);
         end if;
         if PLIC_Node /= null then
            Print_DTB_Node (PLIC_Node);
            declare
               PLIC_Reg : Unsigned_64_Array :=
                 Get_Property_Unsigned_64 (PLIC_Node, "reg");
            begin
               Arch.Debug.Print ("PLIC_Reg: parsing PLIC node");
               if PLIC_Reg'Length >= 2 then
                  Arch.PLIC.Set_PLIC_Configuration (
                    Base_Address =>
                    System.Storage_Elements.To_Address (
                     U64_To_Int_Addr (PLIC_Reg (1))),
                    Priority_Offset     => Unsigned_64 (0),
                    Context_Base_Offset => PLIC_Reg (2),
                    Context_Stride      => Unsigned_64 (16#1000#),
                    Threshold_Offset    => Unsigned_64 (0),
                    Max_Interrupt_ID    => Unsigned_64 (1023),
                    Max_Harts           => Num_Harts,
                    Contexts_Per_Hart   => Unsigned_64 (1),
                    Enabled             => True);
                  Arch.Debug.Print (
                    "PLIC configured from DTB and SMP info.");
               else
                  Arch.Debug.Print (
                    "PLIC DTB info incomplete; using defaults.");
                  Arch.PLIC.Set_PLIC_Configuration;
               end if;
            end;
         else
            Arch.Debug.Print (
              "PLIC node not found; using defaults.");
            Arch.PLIC.Set_PLIC_Configuration;
         end if;
      exception
         when others =>
            Debug.Print ("Exception occurred during PLIC config.");
            Lib.Panic.Hard_Panic ("PLIC configuration failed");
      end;

      --  9. Interrupt init
      begin
         Arch.Debug.Print (
           "Initializing interrupt controllers for " &
           Unsigned_64'Image (Num_Harts) & " cores");
         Arch.Interrupts.Initialize;
         Arch.Debug.Print ("Interrupt controllers initialized");
      exception
         when others =>
            Debug.Print (
               "Exception occurred during interrupt init.");
            Lib.Panic.Hard_Panic (
               "Interrupt initialization failed");
      end;

      --  10. Trap vector
      begin
         Arch.Debug.Print ("Setting trap entry vector");
         Arch.CPU.Set_Trap_Vector;
      exception
         when others =>
            Debug.Print (
               "Exception occurred while setting trap vector");
            Lib.Panic.Hard_Panic (
               "Trap vector setup failed");
      end;

      --  11. Command line & Main
      begin
         Debug.Print ("Copying command line");
         Arch.Cmdline_Len := Info.Cmdline_Len;
         Arch.Cmdline (1 .. Info.Cmdline_Len)
            := Info.Cmdline (1 .. Info.Cmdline_Len);
         Debug.Print ("Command line copied");
         Debug.Print ("Jumping to main kernel");
         Main;
      exception
         when others =>
            Debug.Print (
               "Exception occurred during command"
               & " line setup or main execution");
            Lib.Panic.Hard_Panic (
               "Command line setup or main execution failed");
      end;
   end Bootstrap_Main;

end Arch.Entrypoint;