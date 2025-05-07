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
with Arch.Hooks;
with Arch.Limine;
with Lib.Messages;      use Lib.Messages;
with Memory.Physical;
with Arch.MMU;
with Lib.Panic;
with Arch.FDT;
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
      Buffer : Context_String := (others => ' ');
      I      : Integer        := Buffer'Last;
      N      : Unsigned_64    := Value;
   begin
      if N = 0 then
         Buffer (I) := '0';
      else
         while N > 0 loop
            Buffer (I) := Character'Val
                           (Character'Pos('0') + Integer(N mod 10));
            N := N / 10;
            I := I - 1;
         end loop;
      end if;

      return Buffer;
   exception
      when Constraint_Error =>
         Arch.Debug.Print ("[Error] Constraint error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Constraint error in Unsigned_To_String");
      when Program_Error =>
         Arch.Debug.Print ("[Error] Program error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Program error in Unsigned_To_String");
      when Storage_Error =>
         Arch.Debug.Print ("[Error] Storage error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Storage error in Unsigned_To_String");
      when others =>
         Arch.Debug.Print ("[Error] Unknown error in Unsigned_To_String");
         Lib.Panic.Hard_Panic ("Unknown error in Unsigned_To_String");
   end Unsigned_To_String;

   ----------------------------------------------------------------------
   --  Centralized exception handler for better debugging
   ----------------------------------------------------------------------
   procedure Handle_Exception (Context : String) is
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
         if not Arch.Hooks.Devices_Hook then
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

      --  declare
      --     Received_Line : String (1 .. 100);
      --  begin
      --     Devices.UART.Read_UART0_Line(Received_Line);
      --     Lib.Messages.Put_Line(Received_Line);
      --  end;

      -- Allocators & MMU Initialization
      begin
         Arch.Debug.Print ("[Stage 4] Initializing allocators and MMU...");
         Memory.Physical.Init_Allocator (Info.Memmap (Info.Memmap'Range));
         Arch.Debug.Print ("[Stage 4] Physical allocator initialized");
         if not Arch.MMU.Init (Info.Memmap) then
            Lib.Panic.Hard_Panic ("The VMM could not be initialized");
         end if;
         Arch.Debug.Print ("[Stage 4] MMU initialized successfully");
      exception
         when others => Handle_Exception ("Allocator or MMU initialization");
      end;

      --  Initialize device discovery facilities.
      Debug.Print ("Initializing DTB discovery");
      -- if not Arch.DTB.Init then
      --   Lib.Panic.Hard_Panic ("No DTB was found!");
      -- end if;



      --  Initialize the allocators and MMU.
      Debug.Print ("Initializing allocators and MMU");
      Lib.Messages.Put_Line ("Initializing allocators");
      Memory.Physical.Init_Allocator (Info.Memmap (1 .. Info.Memmap_Len));
      Debug.Print ("Physical allocator initialized");
      Debug.Print ("Initializing MMU");
      if not Arch.MMU.Init (Info.Memmap (1 .. Info.Memmap_Len)) then
         Lib.Panic.Hard_Panic ("The VMM could not be initialized");
      end if;
      Debug.Print ("MMU initialized");

            Lib.Messages.Put_Line ("allocator initialized");

      --  Enable dmesg buffers and KASAN if wanted.
      Debug.Print ("Enabling logging");
      Lib.Messages.Enable_Logging;
      Debug.Print ("Logging enabled");
      #if KASAN
         Debug.Print ("Initializing KASAN");
         Lib.KASAN.Init;
         Debug.Print ("KASAN initialized");
      #end if;

      --  Print the memory map, it is useful at times.
      Lib.Messages.Put_Line ("Physical memory map:");
      for E of Info.Memmap (1 .. Info.Memmap_Len) loop
         Addr := E.Start + E.Length;
         Lib.Messages.Put_Line
            ("[" & E.Start'Image & " - " & Addr'Image & "] " &
             Boot_Memory_Type'Image (E.MemType));
      end loop;

      --  Initialize the other cores of the system.
      Debug.Print ("Initializing cores");
      Arch.CPU.Init_Cores;
      Debug.Print ("Cores initialized");

      --  Go to main kernel.
      Debug.Print ("Copying command line");
      Arch.Cmdline_Len := Info.Cmdline_Len;
      Arch.Cmdline (1 .. Info.Cmdline_Len) :=
         Info.Cmdline (1 .. Info.Cmdline_Len);
      Debug.Print ("Command line copied");
      Debug.Print ("Jumping to main kernel");
      Main;
   end Bootstrap_Main;

end Arch.Entrypoint;
