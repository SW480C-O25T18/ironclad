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

with Interfaces;           use Interfaces;
with System;               use System;
with System.Machine_Code;  use System.Machine_Code;

package body Arch.SBI is

   ----------------------------------------------------------------------------
   --  Probe_Extension : returns True if an SBI extension is supported.
   ----------------------------------------------------------------------------
   function Probe_Extension (Ext : Extension_Id) return Boolean is
      Result : Unsigned_64;
   begin
      -- A0 = Ext, A6 = 0 (probe function), A7 = Base extension EID (0x10)
      Asm ("mv   a0, %0",  Inputs   => (Ext));
      Asm ("li   a6, %0",  Inputs   => (Unsigned_64 (0)));
      Asm ("li   a7, %0",  Inputs   => (Unsigned_64 (16#10#)));
      Asm ("ecall",        Clobbers => ("a0","a6","a7"));
      Asm ("mv   %0, a0",  Outputs  => (Result));
      return Result /= 0;
   end Probe_Extension;

   ----------------------------------------------------------------------------
   --  Set_Timer : program the next timer interrupt at absolute time.
   ----------------------------------------------------------------------------
   procedure Set_Timer (Next_Time : Unsigned_64) is
   begin
      if Probe_Extension (Extension_Timer_Str) then
         -- v0.2+ TIME extension: FID=1
         Asm ("mv   a0, %0",  Inputs => (Next_Time));
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (1)));
         Asm ("li   a7, %0",  Inputs => (Extension_Timer_Str));
      else
         -- legacy TIME extension: FID=0
         Asm ("mv   a0, %0",  Inputs => (Next_Time));
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (0)));
         Asm ("li   a7, %0",  Inputs => (Extension_Timer));
      end if;
      Asm ("ecall");
   end Set_Timer;

   ----------------------------------------------------------------------------
   --  Get_Time : read the current time via SBI (or 0 if unsupported).
   ----------------------------------------------------------------------------
   function Get_Time return Unsigned_64 is
      Result : Unsigned_64;
   begin
      if Probe_Extension (Extension_Timer_Str) then
         -- v0.2+ TIME extension: FID=0 => current time
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (0)));
         Asm ("li   a7, %0",  Inputs => (Extension_Timer_Str));
         Asm ("ecall");
         Asm ("mv   %0, a0",  Outputs => (Result));
         return Result;
      else
         return 0;
      end if;
   end Get_Time;

   ----------------------------------------------------------------------------
   --  Send_Ipi : send software interrupt to a hart‐mask.
   ----------------------------------------------------------------------------
   procedure Send_Ipi (Hart_Mask_Ptr : Address) is
   begin
      if Probe_Extension (Extension_Ipi_Str) then
         -- v0.2+ IPI extension: FID=0
         Asm ("mv   a0, %0",  Inputs => (Hart_Mask_Ptr));
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (0)));
         Asm ("li   a7, %0",  Inputs => (Extension_Ipi_Str));
      else
         -- legacy IPI extension: FID=0
         Asm ("mv   a0, %0",  Inputs => (Hart_Mask_Ptr));
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (0)));
         Asm ("li   a7, %0",  Inputs => (Extension_Ipi));
      end if;
      Asm ("ecall");
   end Send_Ipi;

   ----------------------------------------------------------------------------
   --  Remote_Fence : send remote‐fence IPI, if supported.
   ----------------------------------------------------------------------------
   procedure Remote_Fence (Hart_Mask_Ptr : Address) is
   begin
      if Probe_Extension (Extension_Fence_Ipi) then
         -- fence‐IPI extension: FID=0
         Asm ("mv   a0, %0",  Inputs => (Hart_Mask_Ptr));
         Asm ("li   a6, %0",  Inputs => (Unsigned_64 (0)));
         Asm ("li   a7, %0",  Inputs => (Extension_Fence_Ipi));
         Asm ("ecall");
      end if;
   end Remote_Fence;

end Arch.SBI;