--  arch-clint.adb: CLINT driver body for riscv64-limine
--  Copyright (C) 2025 Sean Weeks
--
--  This file is part of Ironclad.
--  Ironclad is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  Ironclad is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Ironclad.  If not, see <https://www.gnu.org/licenses/>.

with Arch.CPU;
with Arch.Debug;
with Arch.FDT;
with Arch.SBI;
with Interfaces;
with Lib.Panic;
with System;
with System.Storage_Elements;

package body Arch.CLINT with SPARK_Mode => Off is
   pragma Restrictions (No_Exception_Propagation);

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   subtype Address is System.Address;

   --  Helper: read 64-bit little-endian
   function Read_LE64 (Ptr : Address) return U64 is
      B    : U64 := 0;
      Byte : Interfaces.Unsigned_8;
   begin
      for I in 0 .. 7 loop
         Byte := Interfaces.Unsigned_8(
            System.Storage_Elements.Read (Ptr + I)
         );
         B := B or Interfaces.Shift_Left (U64 (Byte), I * 8);
      end loop;
      return B;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Read_LE64 at " & Ptr'Image);
         return 0;
   end Read_LE64;

   --  Helper: write 32-bit little-endian
   procedure Write_LE32 (Ptr : Address; Value : U32) is
      V : U32 := Value;
   begin
      for I in 0 .. 3 loop
         System.Storage_Elements.Write (
            Ptr + I,
            Interfaces.Unsigned_8 (V mod 16#100#)
         );
         V := V / 16#100#;
      end loop;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Write_LE32 at " & Ptr'Image);
   end Write_LE32;

   --  Helper: write 64-bit little-endian
   procedure Write_LE64 (Ptr : Address; Value : U64) is
      V : U64 := Value;
   begin
      for I in 0 .. 7 loop
         System.Storage_Elements.Write (
            Ptr + I,
            Interfaces.Unsigned_8 (V mod 16#100#)
         );
         V := Interfaces.Shift_Right (V, 8);
      end loop;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Write_LE64 at " & Ptr'Image);
   end Write_LE64;

   --  Compute MSIP address
   function MSIP_Addr (Hart : U32) return Address is
   begin
      return MSIP_Base + Address (Hart * MSIP_Stride);
   end MSIP_Addr;

   --  Compute MTIMECMP address
   function MTIMECMP_Addr (Hart : U32) return Address is
   begin
      return MTIMECMP_Base + Address (Hart * MTIMECMP_Stride);
   end MTIMECMP_Addr;

   --------------------------------------------------------------------------
   --  Initialization: detect SBI or MMIO, configure timer interval
   --------------------------------------------------------------------------
   procedure Initialize is
      Handle   : Arch.FDT.Handle;
      Node     : Address;
      Regs     : Arch.FDT.Reg_Vector;
      Timebase : U32;
      --  scheduler quantum in μs
      Quantum  : U32 := Scheduler.Cluster_Pool(
                     Scheduler.Cluster_Pool'First
                  ).RR_Quantum;
   begin
      --  Probe timer SBI extensions
      if Arch.SBI.Probe_Extension (Current_EID_TIME) then
         Use_SBI_Time := True;
      elsif Arch.SBI.Probe_Extension (Legacy_EID_Time) then
         Use_SBI_Time := True;
      else
         Use_SBI_Time := False;
      end if;

      --  Probe IPI SBI extensions
      if Arch.SBI.Probe_Extension (Current_EID_sPI) then
         Use_SBI_IPI := True;
      elsif Arch.SBI.Probe_Extension (Legacy_EID_SendIPI) then
         Use_SBI_IPI := True;
      else
         Use_SBI_IPI := False;
      end if;

      --  Discover MMIO via FDT
      Handle := Arch.FDT.Open (Arch.CPU.Self_Hart'Address);
      Node   := Arch.FDT.Find_Node (Handle, "riscv,clint0");
      if Node /= System.Null_Address then
         Regs := Arch.FDT.Get_Reg (Handle, Node);
         if Regs.Length >= 1 then
            Base_Address  := System.Storage_Elements.Integer_To_Address(
                               U64 (Regs.Entries (1).Address)
                            );
            MSIP_Base     := Base_Address;
            MTIME_Base    := Base_Address + MTIME_Offset;
            MTIMECMP_Base := Base_Address + MTIMECMP_Offset;
            Use_MMIO_Time := True;
            Use_MMIO_IPI  := True;
         end if;
      end if;

      CLINT_Enabled := Use_SBI_Time or Use_MMIO_Time
                    and Use_SBI_IPI or Use_MMIO_IPI;

      --  Configure timer interval if MMIO or SBI timer present
      if Use_MMIO_Time or Use_SBI_Time then
         Timebase := Arch.FDT.Get_Property_U32 (
                        Handle, Node,
                        "timebase-frequency", 0
                     );
         if Timebase = 0 then
            Lib.Panic.Hard_Panic ("CLINT missing timebase-frequency");
         end if;
         Timer_Interval := U64 (Timebase) * U64 (Quantum) / 1_000_000;
      end if;
   exception
      when others =>
         Lib.Panic.Hard_Panic ("CLINT.Initialize panicking");
   end Initialize;

   function Is_Enabled return Boolean is
   begin
      return CLINT_Enabled;
   end Is_Enabled;

   --------------------------------------------------------------------------
   --  Software IPI API
   --------------------------------------------------------------------------
   procedure Set_Software_Interrupt (Target : U32) is
      Mask : U64 := Interfaces.Shift_Left (1, Target);
   begin
      Arch.Debug.Print ("[TRACE] CLINT Set_Software_Interrupt hart=" & U32'Image (Target));
      if Use_SBI_IPI then
         Arch.SBI.Send_IPI (Mask);
      elsif Use_MMIO_IPI then
         Write_LE32 (MSIP_Addr (Target), 1);
      else
         return;
      end if;
   exception
      when others =>
         Lib.Panic.Hard_Panic ("CLINT Set_Software_Interrupt panicking");
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Target : U32) is
      Mask : U64 := Interfaces.Shift_Left (1, Target);
   begin
      Arch.Debug.Print ("[TRACE] CLINT Clear_Software_Interrupt hart=" & U32'Image (Target));
      if Use_SBI_IPI then
         Arch.SBI.Clear_IPI (Mask);
      elsif Use_MMIO_IPI then
         Write_LE32 (MSIP_Addr (Target), 0);
      else
         return;
      end if;
   exception
      when others =>
         Lib.Panic.Hard_Panic ("CLINT Clear_Software_Interrupt panicking");
   end Clear_Software_Interrupt;

   function Get_Software_Interrupt (Target : U32) return Boolean is
      Val : U32;
   begin
      if Use_SBI_IPI then
         -- no SBI get; fallback to MMIO
         null;
      end if;
      if Use_MMIO_IPI then
         Val := Interfaces.Unsigned_32 (Read_LE64 (MSIP_Addr (Target)));
         return Val /= 0;
      end if;
      return False;
   end Get_Software_Interrupt;

   --------------------------------------------------------------------------
   --  Timer API
   --------------------------------------------------------------------------
   function Read_Timer return U64 is
      Val : U64;
   begin
      if Use_SBI_Time then
         Val := Arch.SBI.Set_Timer; -- incorrect call? Should be Get_Timer if defined
         return Val;
      elsif Use_MMIO_Time then
         return Read_LE64 (MTIME_Base);
      else
         return 0;
      end if;
   end Read_Timer;

   procedure Set_Timer_Interrupt (Value : U64) is
      Hart : U32 := Arch.CPU.Self_Hart;
   begin
      Arch.Debug.Print ("[TRACE] CLINT Set_Timer_Interrupt hart=" & U32'Image (Hart)
                       & " time=" & U64'Image (Value));
      if Use_SBI_Time then
         Arch.SBI.Set_Timer (Value);
      elsif Use_MMIO_Time then
         Write_LE64 (MTIMECMP_Addr (Hart), Value);
      end if;
   exception
      when others =>
         Lib.Panic.Hard_Panic ("CLINT Set_Timer_Interrupt panicking");
   end Set_Timer_Interrupt;

   procedure Clear_Timer_Interrupt is
   begin
      Set_Timer_Interrupt (Read_Timer + Timer_Interval);
   end Clear_Timer_Interrupt;

   --------------------------------------------------------------------------
   --  Resynchronize (no-op)
   --------------------------------------------------------------------------
   procedure Resync (Reference : Address) is
   begin
      if Use_MMIO_Time and not Use_SBI_Time then
         -- Set the MTIMECMP register to the reference value
         Write_LE64 (
            MTIMECMP_Addr (Arch.CPU.Self_Hart), Read_LE64 (Reference));
      end if;
   end Resync;

end Arch.CLINT;