--  arch-clint.adb: Implementation of Core Local Interruptor (CLINT) utilities.
--  Provides functionality for timer and software interrupts using SBI or MMIO.
--  Optimized for RISC-V64 systems with proper exception handling.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
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

with Arch.CPU;                use Arch.CPU;
with Arch.DTB;                use Arch.DTB;
with Arch.SBI;                use Arch.SBI;
with Arch.MMU;                use Arch.MMU;
with Arch.Debug;              use Arch.Debug;
with Lib.Panic;               use Lib.Panic;
with System.Machine_Code;     use System.Machine_Code;

package body Arch.CLINT with SPARK_Mode => Off is

   ----------------------------------------------------------------------------
   --  CLINT MMIO register offsets (per RISC‑V spec)
   ----------------------------------------------------------------------------
   MSIP_Base      : constant Storage_Offset := 0;
   MTIMECMP_Base  : constant Storage_Offset := 16#4000#;
   MTIMECMP_Step  : constant Storage_Offset := 8;
   MTIME_Base     : constant Storage_Offset := 16#BFF8#;

   ----------------------------------------------------------------------------
   --  CLINT state
   ----------------------------------------------------------------------------
   Clint_Base           : Address         := Null_Address;
   Clint_Base_Off       : Storage_Offset  := 0;
   Timebase_Frequency   : Unsigned_64     := 0;
   Core_Count           : Unsigned_64     := 0; -- Initialize to 0
   Has_SBI             : Boolean         := False;
   Has_MMIO            : Boolean         := False;
   Has_CLINT           : Boolean         := False;

   ----------------------------------------------------------------------------
   --  Helper: get an MMIO register’s virtual address
   ----------------------------------------------------------------------------
   function Reg_Addr (Off : Storage_Offset) return Address is
   begin
      return To_Address (Integer_Address (Clint_Base_Off + Off));
   exception
      when others =>
         Debug.Print ("Arch.CLINT: Reg_Addr failed for offset " &
            Storage_Offset'Image (Off));
         return Null_Address;
   end Reg_Addr;

   ----------------------------------------------------------------------------
   --  CLINT availability: prefer SBI, fallback to MMIO
   ----------------------------------------------------------------------------
   function CLINT_Enabled return Boolean is
   begin
      return Has_CLINT;
   end CLINT_Enabled;

   ----------------------------------------------------------------------------
   --  Global configuration: map MMIO or rely on SBI
   ----------------------------------------------------------------------------
   procedure Set_CLINT_Configuration is
      Node    : constant DTB_Node_Access :=
         Find_Node_By_Compatible ("riscv,clint0");
      Base    : Unsigned_64 := 0;
      Size    : Unsigned_64 := 0;
      Phys    : Address := Null_Address;
      Virt    : Address := Null_Address;
      Success : Boolean := False;
   begin
      Debug.Print ("Arch.CLINT: Configuring CLINT");

      -- Determine presence of MMIO and SBI
      Has_SBI := Probe_Extension (Extension_Timer);
      Has_MMIO := Node /= null;

      -- Determine if CLINT is available
      Has_CLINT := Has_SBI or Has_MMIO;

      if not Has_CLINT then
         Debug.Print ("Arch.CLINT: CLINT is not available");
         return;
      end if;

      -- Configure MMIO if available
      if Has_MMIO then
         Debug.Print ("Arch.CLINT: Found DTB node " & Node.Name);
         Base := Get_Property_Unsigned_64 (Node, "reg", 1);
         Size := Get_Property_Unsigned_64 (Node, "reg", 2);
         Phys := To_Address (Integer_Address (Base));
         Virt := Phys;
         Timebase_Frequency := Get_Property_Unsigned_64 (Node, "timebase-frequency", 1);

         Map_Range (
            Map            => Kernel_Table,
            Physical_Start => Phys,
            Virtual_Start  => Virt,
            Length         => Storage_Count (Size),
            Permissions    => (Is_User_Accesible => False,
                              Can_Read          => True,
                              Can_Write         => True,
                              Can_Execute       => False,
                              Is_Global         => True),
            Success        => Success,
            Caching        => Write_Back
         );

         if not Success then
            Debug.Print ("Arch.CLINT: MMIO map failed; disabling CLINT");
            -- Reset global variables to disable CLINT
            Clint_Base          := Null_Address;
            Clint_Base_Off      := 0;
            Has_MMIO            := False;
            Has_CLINT           := Has_SBI; -- Only SBI remains
            Timebase_Frequency  := 0;
            Core_Count          := 0; -- Reset Core_Count
            return;
         end if;

         -- Update global variables
         Clint_Base     := Virt;
         Clint_Base_Off := Storage_Offset (To_Integer (Virt));
         Debug.Print ("Arch.CLINT: MMIO mapped at " & Address'Image (Clint_Base));

         -- Ensure CPU.Core_Count is valid before assigning
         if CPU.Core_Count > 0 then
            Core_Count := CPU.Core_Count;
         else
            Debug.Print ("Arch.CLINT: CPU.Core_Count is invalid; defaulting to 1");
            Core_Count := 1;
         end if;

         Debug.Print ("Arch.CLINT: Number of cores = " & Unsigned_64'Image (Core_Count));
      else
         Debug.Print ("Arch.CLINT: No DTB node found; Using SBI for timer & IPI");
      end if;

   exception
      when others =>
         Debug.Print ("Arch.CLINT: Exception occurred; disabling CLINT");
         -- Reset global variables to disable CLINT
         Clint_Base          := Null_Address;
         Clint_Base_Off      := 0;
         Has_MMIO            := False;
         Has_SBI             := False;
         Has_CLINT           := False;
         Timebase_Frequency  := 0;
         Core_Count          := 0; -- Reset Core_Count
   end Set_CLINT_Configuration;

   ----------------------------------------------------------------------------
   --  Hart-specific initialization: clear MSIP
   ----------------------------------------------------------------------------
   procedure Initialize_Hart (Hart_Id : Unsigned_64) is
      Off : Storage_Offset :=
        Clint_Base_Off + MSIP_Base + Storage_Offset (4 * Integer (Hart_Id));
   begin
      pragma Assert (Hart_Id < Unsigned_64 (Core_Count));
      Debug.Print ("Arch.CLINT: Initializing hart " & Unsigned_64'Image (Hart_Id));

      if Has_SBI then
         Debug.Print ("Arch.CLINT: SBI handles MSIP clear");
         return;
      end if;

      if Has_MMIO then
         declare
            MSIP : Unsigned_32 with Address =>
                     To_Address (Integer_Address (Off));
         begin
            MSIP := 0;
            Debug.Print ("Arch.CLINT: MSIP cleared for hart " & Unsigned_64'Image (Hart_Id));
         end;
      end if;
   exception
      when others =>
         Debug.Print ("Arch.CLINT: Exception in Initialize_Hart");
         if Has_MMIO then
            declare
               MSIP : Unsigned_32 with Address =>
                        To_Address (Integer_Address (Off));
            begin
               MSIP := 0;
               Debug.Print ("Arch.CLINT: MSIP reset to 0 for hart " & Unsigned_64'Image (Hart_Id));
            end;
         end if;
         Lib.Panic.Hard_Panic ("Arch.CLINT: Failed to initialize hart");
   end Initialize_Hart;

   ----------------------------------------------------------------------------
   --  Timer interrupts
   ----------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64) is
      Off     : Storage_Offset := 0; -- Initialize to a safe default
      HartInt : Integer := Integer (Read_Hart_ID);
   begin
      if Has_SBI then
         Debug.Print ("Arch.CLINT: Setting SBI timer to " & Unsigned_64'Image (Next_Time));
         Arch.SBI.Set_Timer (Next_Time);
      elsif Has_MMIO then
         Off := Clint_Base_Off + MTIMECMP_Base + MTIMECMP_Step * Storage_Offset (HartInt);
         declare
            CMP : Unsigned_64 with Address =>
                  To_Address (Integer_Address (Off));
         begin
            CMP := Next_Time;
            Debug.Print ("Arch.CLINT: MTIMECMP set to " & Unsigned_64'Image (Next_Time));
         end;
      end if;
   exception
      when others =>
         Debug.Print ("Arch.CLINT: Exception in Set_Timer");
         if Has_MMIO and Off /= 0 then
            declare
               CMP : Unsigned_64 with Address =>
                     To_Address (Integer_Address (Off));
            begin
               CMP := 0;
               Debug.Print ("Arch.CLINT: MTIMECMP reset to 0");
            end;
         end if;
   end Set_Timer;

   ----------------------------------------------------------------------------
   --  Read current time (for Reschedule_In and other uses)
   ----------------------------------------------------------------------------
   function Read_Timer return Unsigned_64 is
   begin
      if Has_SBI then
         Debug.Print ("Arch.CLINT: Reading timer using SBI");
         return Get_Time;
      elsif Has_MMIO then
         Debug.Print ("Arch.CLINT: Reading timer using MMIO");
         declare
            MTIME : Unsigned_64 with Address =>
                     To_Address (Integer_Address (Clint_Base_Off + MTIME_Base));
         begin
            return MTIME;
         exception
            when others =>
               Debug.Print ("Arch.CLINT: Exception occurred while reading MMIO timer");
               return 0; -- Return a default value in case of failure
         end;
      else
         return 0; -- No timer available
      end if;
   end Read_Timer;

end Arch.CLINT;