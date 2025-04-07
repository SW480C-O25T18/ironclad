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

--  #if KASAN
--     with Lib.KASAN;
--  #end if;

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
      Info      : Boot_Information renames Limine.Global_Info;
      Addr      : System.Address;
      Num_Harts : Unsigned_64;
      -- Declare local variables for DTB nodes and properties without initializing them here.
      CLINT_Node : DTB_Node_Access;
      PLIC_Node  : DTB_Node_Access;
      CLINT_Reg  : Unsigned_64_Array;
      PLIC_Reg   : Unsigned_64_Array;
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
      --  #if KASAN
      --     Debug.Print ("Initializing KASAN");
      --     Lib.KASAN.Init;
      --     Debug.Print ("KASAN initialized");
      --  #end if;

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

      ------------------------------------------------------------------------------
      -- Configure CLINT and PLIC using DTB and SMP information.
      -- For the CLINT, we retrieve the "reg" property from the node with a compatible
      -- string "riscv,clint". For the PLIC, we retrieve the "reg" property from the node
      -- with compatible "riscv,plic" and use Num_Harts to set the number of harts.
      ------------------------------------------------------------------------------
      Arch.Debug.Print ("Search for CLINT node in DTB");
      --  The CLINT node is searched by compatible string "riscv,clint".
      --  If not found, fallback to "riscv,interrupt-controller".
      --  This is a common pattern in DTBs for RISC-V systems.
      CLINT_Node := Arch.DTB.Find_Node_By_Compatible("riscv,clint");
      if CLINT_Node = null then
         CLINT_Node := Arch.DTB.Find_Node_By_Compatible("riscv,interrupt-controller");
         Arch.Debug.Print ("CLINT_Node (fallback): " & CLINT_Node'Image);
      end if;
      -- output the node for debugging
      if CLINT_Node /= null then
         Print_DTB_Node(CLINT_Node);
      else
         Arch.Debug.Print("No DTB root node available.");
      end if;
      -- The CLINT_Node is configured with the "reg" property.
      -- The "reg" property is expected to contain the base address and offsets for MSIP, MTime, and MTimecmp.
      if CLINT_Node /= null then
         CLINT_Reg := Arch.DTB.Get_Property_Unsigned_64(CLINT_Node, "reg");
         if CLINT_Reg'Length >= 4 then
            Arch.CLINT.Set_CLINT_Configuration(
               Base_Address    => To_Address(CLINT_Reg(1)),
               MSIP_Offset     => CLINT_Reg(2),
               MTime_Offset    => CLINT_Reg(3),
               MTimecmp_Offset => CLINT_Reg(4),
               Enabled         => True);
            Arch.Debug.Print("CLINT configured from DTB.");
         else
            Arch.Debug.Print("CLINT DTB information incomplete; using defaults.");
            Arch.CLINT.Set_CLINT_Configuration;
         end if;
      else
         Arch.Debug.Print("CLINT node not found in DTB; using defaults.");
         Arch.CLINT.Set_CLINT_Configuration;
      end if;

      Arch.Debug.Print ("Search for PLIC node in DTB");
      --  The PLIC node is searched by compatible string "riscv,plic" or fallback to "riscv,interrupt-controller".
      PLIC_Node := Arch.DTB.Find_Node_By_Compatible("riscv,plic");
      if PLIC_Node = null then
         PLIC_Node := Arch.DTB.Find_Node_By_Compatible("riscv,interrupt-controller");
         Arch.Debug.Print ("PLIC_Node (fallback): " & PLIC_Node'Image);
      end if;
      -- output the node for debugging
      if PLIC_Node /= null then
         Print_DTB_Node(PLIC_Node);
      else
         Arch.Debug.Print("No DTB root node available.");
      end if;
      -- The PLIC_Node is configured with the "reg" property.
      if PLIC_Node /= null then
         PLIC_Reg := Arch.DTB.Get_Property_Unsigned_64(PLIC_Node, "reg");
         if PLIC_Reg'Length >= 2 then
            Arch.PLIC.Set_PLIC_Configuration(
               Base_Address         => To_Address(PLIC_Reg(1)),
               Priority_Offset      => 0,  -- Assume priority registers start at offset 0
               Context_Base_Offset  => PLIC_Reg(2),
               Context_Stride       => 16#1000#,  -- Default stride (adjust if DTB provides a value)
               Threshold_Offset     => 0,
               Max_Interrupt_ID     => 1023,
               Max_Harts            => Num_Harts,  -- Use the number of harts from SMP
               Contexts_Per_Hart    => 1,          -- Default; adjust if needed
               Enabled              => True);
            Arch.Debug.Print("PLIC configured from DTB and SMP response.");
         else
            Arch.Debug.Print("PLIC DTB information incomplete; using defaults.");
            Arch.PLIC.Set_PLIC_Configuration;
         end if;
      else
         Arch.Debug.Print("PLIC node not found in DTB; using defaults.");
         Arch.PLIC.Set_PLIC_Configuration;
      end if;

      ------------------------------------------------------------------------------
      -- Initialize interrupt controllers (per core initialization).
      -- This is done after the PLIC and CLINT have been configured.
      -- The interrupt controllers are initialized for each core in the system.
      ------------------------------------------------------------------------------
      Arch.Debug.Print("Initializing interrupt controllers for " & Unsigned_64'Image(Num_Harts) & " cores");
      Arch.Interrupts.Initialize;
      Arch.Debug.Print("Interrupt controllers initialized");

      ------------------------------------------------------------------------------
      -- Set the trap entry point early, so that any trap is handled correctly.
      -- This is done by writing the address of the trap_entry procedure to stvec.
      -- Setting the trap vector after interrrupts are initialized so that they can be
      -- handled correctly.
      ------------------------------------------------------------------------------
      Arch.Debug.Print("Setting trap entry vector");
      Arch.CPU.Set_Trap_Vector;  -- This procedure writes trap_entry's address to stvec.

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
