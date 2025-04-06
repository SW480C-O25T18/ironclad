--  arch-entrypoint.adb: Limine plops us here.
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

with Arch.Debug;
with Interfaces; use Interfaces;
with Devices.UART;
with Arch.Limine;
with Lib.Messages; use Lib.Messages;
with Memory.Physical;
with Arch.MMU;
with Lib.Panic;
with Arch.DTB;
with Arch.CPU;
with Main;
with Arch.Interrupts;
with Arch.Local;

#if KASAN
   with Lib.KASAN;
#end if;

package body Arch.Entrypoint is

   ------------------------------------------------------------------------------
   -- Bootstrap_Main
   --
   -- The kernel entrypoint as provided by Limine. This procedure performs
   -- early hardware initialization, including:
   --   1. Initializing UART for early debug output.
   --   2. Translating the Limine protocol into architecture-agnostic structures.
   --   3. Parsing the DTB to extract hardware configuration.
   --   4. Initializing the physical memory allocator and MMU.
   --   5. Enabling logging (and KASAN if configured).
   --   6. Printing the physical memory map.
   --   7. Initializing CPU cores and their local state.
   --   8. Initializing the interrupt controllers (PLIC and CLINT) dynamically.
   --   9. Copying the kernel command line.
   --  10. Jumping to the main kernel entry point.
   --
   -- This procedure is called as soon as control is transferred from the bootloader.
   ------------------------------------------------------------------------------
   procedure Bootstrap_Main is
      Info : Boot_Information renames Limine.Global_Info;
      Addr : System.Address;
      Num_Harts : Unsigned_64;
   begin
      -- Initialize basic hardware state.
      Devices.UART.Init_UART0;
      Lib.Messages.Put_Line("Hello from kernel entrypoint");

      -- Translate the Limine protocol into architecture-agnostic structures.
      Arch.Debug.Print("Translating Limine protocol");
      Limine.Translate_Proto;

      -- Initialize the Device Tree Blob (DTB) to obtain hardware configuration.
      Arch.Debug.Print("Initializing DTB discovery");
      if not Arch.DTB.Init then
         Lib.Panic.Hard_Panic("No DTB was found!", System.Null_Address);
      end if;
      Arch.Debug.Print("DTB initialized successfully");

      --  Initialize device discovery facilities.
      Debug.Print ("Initializing DTB discovery");
      if not Arch.DTB.Init then
         Lib.Panic.Hard_Panic ("No DTB was found!");
      end if;

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

      -- Initialize CPU cores.
      Arch.Debug.Print("Initializing CPU cores");
      Arch.CPU.Init_Cores;
      Num_Harts := Arch.CPU.Core_Count;
      Arch.Debug.Print("CPU cores initialized: " & Unsigned_64'Image(Num_Harts));

      -- Configure interrupt controllers using DTB-derived (or default) values.
      Arch.Debug.Print("Configuring CLINT and PLIC");
      Arch.CLINT.Set_CLINT_Configuration;
      Arch.PLIC.Set_PLIC_Configuration;
      Arch.Debug.Print("CLINT and PLIC configured");

      -- Initialize the interrupt controllers (CLINT and PLIC) for each core.
      Arch.Debug.Print("Initializing interrupt controllers for " & Unsigned_64'Image(Num_Harts) & " cores");
      Arch.Interrupts.Initialize;
      Arch.Debug.Print("Interrupt controllers initialized");

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
