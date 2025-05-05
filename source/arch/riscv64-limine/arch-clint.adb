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

with System;
with Interfaces;
with System.Storage_Elements;
with Arch.Clint;
with Arch.CPU;
with Arch.Debug;
with Lib.Panic;
with Arch.SBI;
with Arch.FDT;

package body Arch.Clint is
   pragma Restrictions (No_Exception_Propagation);

   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   subtype Address is System.Address;

   --  Helpers for little-endian writes and reads
   function Read_BE64 (Ptr : Address) return U64 is
   begin
      return U64 (System.Storage_Elements.Read (Ptr)) or else
             U64 (System.Storage_Elements.Read (
                  Ptr + 1)) * 16#100# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 2)) * 16#10000# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 3)) * 16#1000000# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 4)) * 16#100000000# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 5)) * 16#10000000000# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 6)) * 16#1000000000000# or else
             U64 (System.Storage_Elements.Read (
               Ptr + 7)) * 16#100000000000000#;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Read_BE64 failed");
         Lib.Panic.Hard_Panic ("CLINT read error");
   end Read_BE64;

   procedure Write_LE32 (Ptr : Address; Value : U32) is
      B0 : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (
         Value and 16#FF#);
      B1 : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (
         (Value / 16#100#) and 16#FF#);
      B2 : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (
         (Value / 16#10000#) and 16#FF#);
      B3 : Interfaces.Unsigned_8 := Interfaces.Unsigned_8 (
         (Value / 16#1000000#) and 16#FF#);
   begin
      System.Storage_Elements.Write (
         Ptr,     System.Storage_Elements.Storage_Unit (B0));
      System.Storage_Elements.Write (
         Ptr + 1, System.Storage_Elements.Storage_Unit (B1));
      System.Storage_Elements.Write (
         Ptr + 2, System.Storage_Elements.Storage_Unit (B2));
      System.Storage_Elements.Write (
         Ptr + 3, System.Storage_Elements.Storage_Unit (B3));
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Write_LE32 failed");
         Lib.Panic.Hard_Panic ("CLINT write error");
   end Write_LE32;

   --  Initialize CLINT bases via DTB
   procedure Initialize is
      H     : Arch.FDT.Handle;
      Node  : System.Address;
      Regs  : Arch.FDT.Reg_Vector;
   begin
      H    := Arch.FDT.Open (Arch.FDT.Base_Address);
      Node := Arch.FDT.Find_Node (H, "riscv,clint0");
      if Node = System.Null_Address then
         Arch.Debug.Print ("[WARN] CLINT node not found");
         return;
      end if;
      Regs := Arch.FDT.Get_Reg (H, Node);
      if Regs.Length < 2 then
         Arch.Debug.Print ("[ERROR] CLINT reg entries incomplete");
         Lib.Panic.Hard_Panic ("CLINT reg missing");
      end if;
      --  Per spec: first entry = mtime, second = mtimecmp base
      MTIME_Base    := System.Storage_Elements.Integer_To_Address (
                        Interfaces.Unsigned_64 (Regs.Entries (1).Address));
      MTIMECMP_Base := System.Storage_Elements.Integer_To_Address (
                        Interfaces.Unsigned_64 (Regs.Entries (2).Address));
      --  MSIP region starts at mtimecmp_base - 0x4000
      MSIP_Base     :=MTIMECMP_Base - Interfaces.Unsigned_64 (
         Arch.Clint.MTIMECMP_Stride * 0);
      Arch.Debug.Print ("[TRACE] CLINT bases: MSIP=" & MSIP_Base'Image &
                        ", MTIME=" & MTIME_Base'Image &
                        ", MTIMECMP=" & MTIMECMP_Base'Image);
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] CLINT Initialize failed");
         Lib.Panic.Hard_Panic ("CLINT Init error");
   end Initialize;

   --  Software IPI
   procedure Set_Software_Interrupt (Target : U32) is
      Mask : U64 := Interfaces.Shift_Left (1#0#, Target);
   begin
      if Arch.SBI.Probe_Extension (16#03#) then
         Arch.SBI.Send_IPI (Mask);
      else
         Write_LE32 (MSIP_Base + Address (
            Targets => Target * Arch.Clint.MSIP_Stride), 1#0#);
      end if;
   end Set_Software_Interrupt;

   procedure Clear_Software_Interrupt (Target : U32) is
      Mask : U64 := Interfaces.Shift_Left(1#0#, Target);
   begin
      if Arch.SBI.Probe_Extension (16#03#) then
         Arch.SBI.Clear_IPI (Mask);
      else
         Write_LE32 (MSIP_Base + Address (
            Targets => Target * Arch.Clint.MSIP_Stride), 0#0#);
      end if;
   end Clear_Software_Interrupt;

   function Get_Software_Interrupt (Target : U32) return Boolean is
      Val  : U32;
   begin
      --  MSIP is 32-bit WARL; read LSB at offset 0
      Val := Interfaces.Unsigned_32 (System.Storage_Elements.Read (
               MSIP_Base + Address (
                  Targets => Target * Arch.Clint.MSIP_Stride)));
      return (Val and 1#0#) /= 0#0#;
   end Get_Software_Interrupt;

   --  Timer interrupts
   function Read_Timer return U64 is
   begin
      return Read_BE64 (MTIME_Base);
   end Read_Timer;

   procedure Set_Timer_Interrupt (Time_Value : U64) is
   begin
      if Arch.SBI.Probe_Extension (16#00#) then
         Arch.SBI.Set_Timer (Time_Value);
      else
         -- write compare for current hart
         Write_LE32 (Arch.Clint.MTIMECMP_Base +
                     Address (
                        System.Storage_Elements.Integer_To_Address (
                           Arch.Clint.MTIMECMP_Stride *
                              Arch.Clint.Self_Hart)),
                     Interfaces.Unsigned_32 (Time_Value and 16#FFFFFFFF#));
      end if;
   end Set_Timer_Interrupt;

   procedure Clear_Timer_Interrupt is
      Now : U64 := Read_Timer;
   begin
      Set_Timer_Interrupt (Now + 1#0#);
   end Clear_Timer_Interrupt;

   --  Resynchronize MTIME (stub)
   procedure Resync (Reference : Address) is
   begin
      Arch.Debug.Print ("[WARN] CLINT Resync unimplemented");
   end Resync;

end Arch.Clint;
