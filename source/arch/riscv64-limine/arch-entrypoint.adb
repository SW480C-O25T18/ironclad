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

#if KASAN
   with Lib.KASAN;
#end if;

package body Arch.Entrypoint is
   procedure Bootstrap_Main is
      Info : Boot_Information renames Limine.Global_Info;
      Addr : System.Address;
   begin
      --  Initialize architectural state first.
      Devices.UART.Init_UART0;

      Lib.Messages.Put_Line ("Hello");

      --  declare
      --     Received_Line : String (1 .. 100);
      --  begin
      --     Devices.UART.Read_UART0_Line(Received_Line);
      --     Lib.Messages.Put_Line(Received_Line);
      --  end;

      --  Translate the limine protocol into arch-agnostic structures.
      Limine.Translate_Proto;


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
