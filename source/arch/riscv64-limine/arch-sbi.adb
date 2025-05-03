--  arch-sbi.adb: Implementation of Supervisor Binary Interface (SBI) utilities.
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

with Interfaces;             use Interfaces;
with System;                 use System;
with System.Machine_Code;    use System.Machine_Code;

package body Arch.SBI is

   --  Legacy SBI call IDs
   Legacy_Timer_FID : constant Unsigned_64 := 0;
   Legacy_Ipi_FID   : constant Unsigned_64 := 4;

   --  Base extension constants
   Base_Ext_EID     : constant Unsigned_64 := 16#10#;
   Probe_Ext_FID    : constant Unsigned_64 := 3;
   Modern_Timer_EID : constant Unsigned_64 := 16#54494D45#;  -- "TIME"
   Modern_Ipi_EID   : constant Unsigned_64 := 16#73504950#;  -- "sIPI"
   Modern_Fence_EID : constant Unsigned_64 := 16#52464E43#;  -- "sfcn"

   ----------------------------------------------------------------------------
   --  Probe_Extension : returns True if extension is supported.
   ----------------------------------------------------------------------------
   function Probe_Extension (Ext : Extension_Id)
     return Boolean is
      Result : Unsigned_64;
   begin
      --  a0 = Ext, a6 = Probe_Ext_FID, a7 = Base_Ext_EID
      Asm ("mv a0, %0",       Inputs => (Ext));
      Asm ("li a6, %0",       Inputs => (Probe_Ext_FID));
      Asm ("li a7, %0",       Inputs => (Base_Ext_EID));
      Asm ("ecall",           Clobbers => ("a0", "a6", "a7"));
      Asm ("mv %0, a0",       Outputs  => (Result));

      return Result /= 0;
   end Probe_Extension;

   ----------------------------------------------------------------------------
   --  Set_Timer : program next timer interrupt.
   ----------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64) is
   begin
      if Probe_Extension (Modern_Timer_EID) then
         Asm ("mv a0, %0",   Inputs => (Next_Time));
         Asm ("li a6, %0",   Inputs => (0));
         Asm ("li a7, %0",   Inputs => (Modern_Timer_EID));
      else
         Asm ("mv a0, %0",   Inputs => (Next_Time));
         Asm ("li a7, %0",   Inputs => (Legacy_Timer_FID));
      end if;
      Asm ("ecall");
   end Set_Timer;

   ----------------------------------------------------------------------------
   --  Send_Ipi : send software interrupt to hart mask.
   ----------------------------------------------------------------------------
   procedure Send_Ipi (Hart_Mask_Ptr : System.Address) is
   begin
      if Probe_Extension (Modern_Ipi_EID) then
         Asm ("mv a0, %0",   Inputs => (Hart_Mask_Ptr));
         Asm ("li a6, %0",   Inputs => (0));
         Asm ("li a7, %0",   Inputs => (Modern_Ipi_EID));
      else
         Asm ("mv a0, %0",   Inputs => (Hart_Mask_Ptr));
         Asm ("li a7, %0",   Inputs => (Legacy_Ipi_FID));
      end if;
      Asm ("ecall");
   end Send_Ipi;

   ----------------------------------------------------------------------------
   --  Remote_Fence : send remote fence IPI (if supported).
   ----------------------------------------------------------------------------
   procedure Remote_Fence (Hart_Mask_Ptr : System.Address) is
   begin
      if Probe_Extension (Modern_Fence_EID) then
         Asm ("mv a0, %0",   Inputs => (Hart_Mask_Ptr));
         Asm ("li a6, %0",   Inputs => (0));
         Asm ("li a7, %0",   Inputs => (Modern_Fence_EID));
         Asm ("ecall");
      end if;
   end Remote_Fence;

end Arch.SBI;
