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
with Arch.Interrupts; use Arch.Interrupts;

package body Arch.CPU with SPARK_Mode => Off is

   SMP_Request : Limine.SMP_Request :=
     ( Base =>
         ( ID       => Limine.SMP_ID,
           Revision => 0,
           Response => System.Null_Address
         ),
       Flags => 0
     )
     with Export, Async_Writers;

   procedure Init_Cores is
      BSP_Hart_ID : Unsigned_64;
      Idx         : Natural := 2;
      SMPPonse : Limine.RISCV64_SMP_Response
        with Import,
             Address => SMP_Request.Base.Response;
   begin
      Debug.Print ("Sending SMP request to Limine");
      if SMP_Request.Base.Response = System.Null_Address then
         Lib.Panic.Hard_Panic ("Limine SMP request needed");
      end if;
      Debug.Print ("Got SMP response from Limine");

      Debug.Print ("Parsing SMP response");
      Core_Count := Natural (SMPPonse.CPU_Count);
      Debug.Print ("Got "
         & Natural'Image (Core_Count)
         & " cores"
      );
      BSP_Hart_ID := SMPPonse.BSP_Hart_ID;
      Debug.Print ("BSP Hart ID: "
         & Unsigned_64'Image (BSP_Hart_ID)
      );

      Debug.Print ("Initializing core locals and BSP");
      Core_Locals := new Core_Local_Arr (1 .. Core_Count);
      Debug.Print ("Core locals initialized");
      Init_Common (1, BSP_Hart_ID);
      Debug.Print ("BSP initialized");

      if Core_Count > 1 then
         declare
            CPUs : Limine.RISCV64_CPU_Info_Arr (1 .. SMPPonse.CPU_Count)
              with Import,
                   Address => SMPPonse.CPUs;
         begin
            Debug.Print ("Initializing other cores");
            for CPU of CPUs loop
               if CPU.Hart_ID /= BSP_Hart_ID then
                  CPU.Extra_Arg := Unsigned_64 (Idx);
                  CPU.Addr      := Core_Bootstrap'Address;
                  Idx := Idx + 1;
               end if;
            end loop;
         end;
      end if;

      for C in Core_Locals'Range loop
         Debug.Print ("Core " & C'Image & " initialized");
      end loop;
   end Init_Cores;

   procedure Core_Bootstrap (
     Info : access Limine.RISCV64_SMP_CPU_Info
   ) is
   begin
      Lib.Messages.Put_Line ("Hello from core "
         & Unsigned_64'Image (Info.Extra_Arg)
      );
      Init_Common (
        Natural (Info.Extra_Arg),
        Info.Hart_ID
      );
      Debug.Print ("Core "
         & Unsigned_64'Image (Info.Hart_ID)
         & " initialized"
      );
   end Core_Bootstrap;

   procedure Init_Common (
     Core_Number : Positive;
     Hart_ID     : Unsigned_64
   ) is
   begin
      Debug.Print ("Init_Common: Core_Number = "
         & Positive'Image (Core_Number)
      );
      Debug.Print ("Init_Common: Hart_ID = "
         & Unsigned_64'Image (Hart_ID)
      );
      Core_Locals (Core_Number) :=
        ( Self            => Core_Locals (Core_Number)'Access,
          Kernel_Stack    => 0,
          User_Stack      => 0,
          Number          => Core_Number,
          Hart_ID         => Hart_ID,
          Current_Thread  => Scheduler.Error_TID,
          Current_Process => Userland.Process.Error_PID
        );
   end Init_Common;

   procedure Set_Trap_Vector is
   begin
      Asm ("csrw stvec, %0",
         Inputs   => Unsigned_64'Asm_Input ("r", trap_entry'Address),
         Volatile => True
      );
   end Set_Trap_Vector;

   function Read_Hart_ID return Unsigned_64 is
      ID : Unsigned_64;
   begin
      Asm ("csrr %0, mhartid",
         Outputs  => Unsigned_64'Asm_Output ("=r", ID),
         Clobber  => "memory",
         Volatile => True
      );
      return ID;
   end Read_Hart_ID;

end Arch.CPU;
