--  arch-cpu.adb: CPU management routines.
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
with System; use System;
with System.Machine_Code; use System.Machine_Code;
with Lib.Messages;
with Lib.Panic;

package body Arch.CPU with SPARK_Mode => Off is
   SMP_Request : Limine.SMP_Request :=
      (Base  =>
         (ID       => Limine.SMP_ID,
          Revision => 0,
          Response => System.Null_Address),
       Flags => 0)
      with Export, Async_Writers;

   procedure Init_Cores is
      BSP_Hart_ID : Unsigned_64;
      Idx         : Natural := 2;

      SMPPonse : Limine.RISCV64_SMP_Response
         with Import, Address => SMP_Request.Base.Response;
   begin
      Debug.Print ("Sending SMP request to Limine");
      --  Check we got a limine answer at all.
      if SMP_Request.Base.Response = System.Null_Address then
         Lib.Panic.Hard_Panic ("Limine SMP request needed");
      end if;
      Debug.Print ("Got SMP response from Limine");

      --  Fetch info.
      Debug.Print ("Parsing SMP response");
      Core_Count  := Natural (SMPPonse.CPU_Count);
      Debug.Print ("Got " & Core_Count'Image & " cores");
      BSP_Hart_ID := SMPPonse.BSP_Hart_ID;
      Debug.Print ("BSP Hart ID: " & BSP_Hart_ID'Image);

      --  Initialize the locals list, and initialize the BSP.
      Debug.Print ("Initializing core locals and BSP");
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);
      Debug.Print ("Core locals initialized");
      Debug.Print ("Initializing BSP");
      Init_Common (1, BSP_Hart_ID);
      Debug.Print ("BSP initialized");

      --  Initialize the other cores.
      if Core_Count > 1 then
         declare
            SMP_CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. SMPPonse.CPU_Count)
               with Import, Address => SMPPonse.CPUs;
         begin
            Debug.Print ("Initializing other cores");
            for CPU of SMP_CPUs loop
               Debug.Print ("Initializing core " & Idx'Image);
               if CPU.Hart_ID /= BSP_Hart_ID then
                  CPU.Extra_Arg := Unsigned_64 (Idx);
                  Debug.Print ("Init_Cores: CPU.Extra_Arg = " & CPU.Extra_Arg'Image);
                  CPU.Addr      := Core_Bootstrap'Address;
                  Debug.Print ("Init_Cores: CPU.Addr = " & CPU.Addr'Image);
                  Idx           := Idx + 1;
                  Debug.Print ("Init_Cores: Idx + 1 = " & Idx'Image);
               end if;
            end loop;
         end;
      end if;
      for cpu in Core_Locals'Range loop
         Debug.Print ("Init_Cores: Core_Locals (" & cpu'Image & ") = " & Core_Locals (cpu)'Image);
         for I in Core_Locals (cpu)'Range loop
            Debug.Print ("Init_Cores: Core_Locals (" & cpu'Image & ") (" & I'Image & ") = " & Core_Locals (cpu) (I)'Image);
         end loop;
      end loop;
      Debug.Print ("All cores initialized");
   end Init_Cores;
   ----------------------------------------------------------------------------
   procedure Core_Bootstrap (Info : access Limine.RISCV64_SMP_CPU_Info) is
   begin
      Lib.Messages.Put_Line ("Hello from core " & Info.Extra_Arg'Image);
      Debug.Print ("Core_Bootstrap: Info.Extra_Arg = " & Info.Extra_Arg'Image);
      Init_Common (Natural (Info.Extra_Arg), Info.Hart_ID);
      Debug.Print ("Core_Bootstrap: Core " & Info.Hart_ID'Image & " initialized");
   end Core_Bootstrap;

   procedure Init_Common (Core_Number : Positive; Hart_ID : Unsigned_64) is
   begin
      Debug.Print ("Init_Common: Core_Number = " & Core_Number'Image);
      Debug.Print ("Init_Common: Hart_ID = " & Hart_ID'Image);
      Core_Locals (Core_Number) :=
         (Self            => Core_Locals (Core_Number)'Access,
          Kernel_Stack    => 0,
          User_Stack      => 0,
          Number          => Core_Number,
          Hart_ID         => Hart_ID,
          Current_Thread  => Scheduler.Error_TID,
          Current_Process => Userland.Process.Error_PID);
      Debug.Print ("Init_Common: Core_Locals (" & Core_Number'Image & ") = " & Core_Locals (Core_Number)'Image);
   end Init_Common;

   ----------------------------------------------------------------------------
   --  Set the trap vector to the entry point of trap_entry.
   --  This is the entry point for all traps and interrupts.
   --  The stvec CSR is used to set the trap vector.
   -- 
   -- The stvec CSR is a 64-bit register that holds the address of the
   -- trap vector. The lower 2 bits of the address are used to determine
   -- the mode of the trap vector. The upper 62 bits are used to hold the
   -- address of the trap vector. The lower 2 bits are set to 0b00 to
   -- indicate that the trap vector is in direct mode. In direct mode,
   -- the address of the trap vector is used directly as the entry point
   -- for the trap handler. In vectored mode, the address of the trap vector
   -- is used as a base address, and the lower 10 bits of the trap number
   -- are used as an offset to determine the entry point for the trap handler.
   --
   -- The stvec CSR is set to the address of the trap_entry function, which
   -- is defined in the arch-trap.S file. 
   -- The trap_entry function is responsible for saving the context of the current 
   -- thread and handling the trap.
   -- The trap_exit function is defined in the arch-trap.S file. It is responsible
   -- for restoring the context of the current thread and returning to the userland process.
   ----------------------------------------------------------------------------
   procedure Set_Trap_Vector is
   begin
      -- Write the address of trap_entry into the stvec CSR.
      Asm("csrw stvec, %0", 
         Inputs   => Unsigned_64'Asm_Input("r", trap_entry'Address),
         Volatile => True);
   end Set_Trap_Vector;

   ----------------------------------------------------------------------------
   --  Read the hart ID of the current core. The hart ID is used to identify
   --  the core in the system.
   ----------------------------------------------------------------------------
   function Read_Hart_ID return Unsigned_64 is
      ID : Unsigned_64;
   begin
      -- Read the mhartid CSR to obtain the current hart (core) ID.
      Asm ("csrr %0, mhartid",
         Outputs  => Unsigned_64'Asm_Output ("=r", ID),
         Clobber  => "memory",
         Volatile => True);
      return ID;
   end Read_Hart_ID;
end Arch.CPU;
