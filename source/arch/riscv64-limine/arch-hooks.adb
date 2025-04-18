--  arch-hooks.adb: Architecture-specific hooks for several utilities.
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

with Devices.UART;
with Arch.Debug;
with Arch.Snippets;
with Arch.CPU; use Arch.CPU;
with Arch.CLINT; use Arch.CLINT;
with Arch.Interrupts;
with Interfaces; use Interfaces;
with Lib.Messages;
with Devices.Ramdev;
with Arch.Limine;
with Ada.Unchecked_Conversion;   -- for Address_To_Unsigned_64
with System;                     -- for System.Address
with System.Machine_Code; use System.Machine_Code;

package body Arch.Hooks is

   -----------------------------------------------------------------------
   -- This helper function converts an address to an Unsigned_64 type. 
   -----------------------------------------------------------------------
   function Address_To_Unsigned_64 is new Ada.Unchecked_Conversion (System.Address, Unsigned_64);

   function Read_TP return Unsigned_64;
   function Write_TP (Value : Unsigned_64) return Boolean;

   function Devices_Hook return Boolean is
   begin
      Debug.Print ("Devices_Hook: Initializing UART0");
      return Devices.UART.Init_UART0;
   end Devices_Hook;

   function PRCTL_Hook (Code : Natural; Arg : System.Address) return Boolean is
      -- Convert Arg to an Unsigned_64 for write operations. 
      Int_Arg : constant Unsigned_64 := Address_To_Unsigned_64 (Arg);
      -- For read operations, treat Arg as a pointer to Unsigned_64. 
      type U64_Ptr is access all Unsigned_64;
      function Addr_To_U64_Ptr is new Ada.Unchecked_Conversion (System.Address, U64_Ptr);
      Ptr : U64_Ptr := Addr_To_U64_Ptr (Arg);
   begin 
      Debug.Print ("PRCTL_Hook: Code = " & Natural'Image(Code) & ", Arg = " & Unsigned_64'Image(Int_Arg));
      case Code is 
         -- Write new value into tp using inline assembly.
         when 1 => return Write_TP (Int_Arg);
         -- Read value from tp into the memory location pointed to by Arg.
         when 2 =>
            declare
               Value : Unsigned_64 := Read_TP;
            begin
               Ptr.all := Value;
               if Ptr.all = Value then
                  Debug.Print("PRCTL_Hook: Verification successful; Ptr.all =  Value.");
                  return True;
               else
                  Debug.Print("PRCTL_Hook: Verification failed; Ptr.all: " & 
                     Unsigned_64'Image(Ptr.all) & 
                     " /= Value: " & Unsigned_64'Image(Value));
                  return False;
               end if;
            end;
         when others =>
            Debug.Print("PRCTL_Hook: Unsupported code " & Natural'Image(Code));
            return False;
      end case;
   end PRCTL_Hook;

   procedure Panic_SMP_Hook is
      -- Obtain the ID of the current hart by reading the mhartid CSR.
      Current_Hart : constant Unsigned_64 := CPU.Read_Hart_ID;
      -- Use the global core count from Arch.CPU as the total number of active harts.
      Hart_Count   : constant Positive    := Arch.CPU.Core_Count;
   begin
      ------------------------------------------------------------------------------
      -- Disable interrupts on the current core.
      -- This prevents further interrupts during the panic sequence.
      ------------------------------------------------------------------------------
      Arch.Snippets.Disable_Interrupts;

      ------------------------------------------------------------------------------
      -- Signal all other harts to enter panic state by sending a software interrupt.
      -- We iterate over all cores (from 0 to Hart_Count - 1), and for each core 
      -- other than the current one, we trigger a software interrupt using the CLINT.
      ------------------------------------------------------------------------------
      for H in 0 .. Hart_Count - 1 loop
         if Unsigned_64(H) /= Current_Hart then
            Arch.CLINT.Set_Software_Interrupt(Unsigned_64(H), True);
            Debug.Print("Panic_SMP_Hook: Sent software interrupt to hart " &
                        Unsigned_64'Image(Unsigned_64(H)));
         else
            Debug.Print("Panic_SMP_Hook: Skipping current hart " &
                        Unsigned_64'Image(Current_Hart));
         end if;
      end loop;

      ------------------------------------------------------------------------------
      -- Print a system-wide panic message.
      ------------------------------------------------------------------------------
      Lib.Messages.Put_Line("Panic_SMP_Hook: Panic: System Halted");

      ------------------------------------------------------------------------------
      -- Halt the current hart (this core halts last).
      -- Arch.Snippets.HCF (Halt and Catch Fire) is used here to put the current 
      -- core into an unrecoverable halt state.
      ------------------------------------------------------------------------------
      Arch.Snippets.HCF;
   exception
      when Constraint_Error =>
         Debug.Print("Panic_SMP_Hook: Constraint_Error encountered");
         null;
   end Panic_SMP_Hook;

   function Get_Active_Core_Count return Positive is
   begin
      Debug.Print ("Arch.Hooks.Get_Active_Core_Count: Getting active core count");
      return Core_Count;
   end Get_Active_Core_Count;

   procedure Register_RAM_Files is
   begin
   Debug.Print ("Register_RAM_Files: Registering RAM files");
      if Devices.Ramdev.Init (Limine.Global_Info.RAM_Files (1 .. Limine.Global_Info.RAM_Files_Len)) then
         Debug.Print ("Register_RAM_Files: RAM files loaded");
      else
         Lib.Messages.Put_Line ("Register_RAM_Files: Could not load RAM files");
      end if;
   exception
      when Constraint_Error =>
         Lib.Messages.Put_Line ("Register_RAM_Files: Errored while loading RAM files");
   end Register_RAM_Files;

   pragma Inline (Read_TP); 
   pragma Inline (Write_TP);

   ----------------------------------------------------------------------
   --  Read and write the TP register. The TP register is used to store the
   -- thread pointer in the RISC-V architecture. The TP register is used to
   --  store the address of the current thread's stack. The TP register is
   --  used to access the thread-local storage (TLS) area for the current
   -- thread.
   ----------------------------------------------------------------------

   ----------------------------------------------------------------------
   --  Read the TP register (x4) via a register‐move
   ----------------------------------------------------------------------
   function Read_TP return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Debug.Print ("Reading TP register");
      Asm ("mv %0, tp",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      Debug.Print ("TP register value: " & Unsigned_64'Image (Value));
      return Value;
   end Read_TP;

   ----------------------------------------------------------------------
   --  Write the TP register (x4) via a register‐move
   ----------------------------------------------------------------------
   function Write_TP (Value : Unsigned_64) return Boolean is
   begin
      Debug.Print("Writing to TP register: " & Unsigned_64'Image(Value));
      Asm ("mv tp, %0",
           Inputs   => Unsigned_64'Asm_Input("r", Value),
           Clobber  => "memory",
           Volatile => True);
      Debug.Print("TP register written");
      return True;
   exception
      when Constraint_Error =>
         Debug.Print("Write_TP: Constraint_Error encountered");
         return False;
   end Write_TP;
end Arch.Hooks;
