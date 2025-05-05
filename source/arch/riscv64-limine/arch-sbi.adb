--  arch-sbi.adb: Supervisor Binary Interface (SBI) wrapper body for riscv64-limine
--  Copyright (C) 2025 Sean Weeks
--
--  This file is part of Ironclad.
--
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

with Interfaces;
with Arch.Debug;
with Lib.Panic;
with System.Machine_Code;

package body Arch.SBI is
   pragma Restrictions (No_Exception_Propagation);

   subtype U64 is Interfaces.Unsigned_64;
   subtype I32 is Interfaces.Unsigned_32;

   -- Low-level SBI ECALL wrapper.
   -- Moves ExtID→a7, FuncID→a6, Arg0–Arg2→a0–a2, then ECALL; result in a0.
   function SBIEcall (
     ExtID  : U64 with Volatile;
     FuncID : U64 with Volatile;
     Arg0   : U64 := 0 with Volatile;
     Arg1   : U64 := 0 with Volatile;
     Arg2   : U64 := 0 with Volatile)
     return U64 is
      Result : U64 with Volatile;
   begin
      pragma Asm (
        "mv a7, %1\n"
        & "mv a6, %2\n"
        & "mv a0, %3\n"
        & "mv a1, %4\n"
        & "mv a2, %5\n"
        & "ecall\n"
        & "mv %0, a0\n",
        Outputs   => Result,
        Inputs    => (ExtID, FuncID, Arg0, Arg1, Arg2),
        Clobbered => ("a7", "a6", "a0", "a1", "a2")
      );
      return Result;
   exception
      when others =>
         Arch.Debug.Print ("[ERROR] SBIEcall failed");
         Lib.Panic.Hard_Panic ("SBI ECALL failure");
   end SBIEcall;

   -- Base Extension (EID=0x10)
   function Get_Spec_Version return U64 is
      Ver : U64 := SBIEcall (16#10#, 0);
   begin
      Arch.Debug.Print ("SBI Spec Version=" & U64'Image (Ver));
      return Ver;
   end Get_Spec_Version;

   function Probe_Extension (EID : U64) return Boolean is
      Ret : U64 := SBIEcall (16#10#, 1, EID);
   begin
      return Ret /= 0;
   end Probe_Extension;

   -- Legacy Console Extension (EID=0x01)
   procedure Console_Putchar (Ch : Character) is
      Code : U64 := U64 (Character'Pos (Ch));
      R    : U64;
   begin
      if not Probe_Extension (16#01#) then
         Arch.Debug.Print ("SBI Console_Putchar unsupported");
         return;
      end if;
      R := SBIEcall (16#01#, 0, Code);
      if R /= 0 then
         Arch.Debug.Print ("Console_Putchar error=" & U64'Image (R));
      end if;
   end Console_Putchar;

   -- Legacy Console Extension (EID=0x02)
   function Console_Getchar return Integer is
      R : U64;
   begin
      if not Probe_Extension (16#02#) then
         Arch.Debug.Print ("SBI Console_Getchar unsupported");
         return -1;
      end if;
      R := SBIEcall (16#02#, 0);
      if Interfaces.Signed_64 (R) < 0 then
         return -1;
      else
         return Integer (R);
      end if;
   end Console_Getchar;

   -- Timer Extension (EID=0x00)
   procedure Set_Timer (Time_Value : U64) is
      R : U64;
   begin
      if not Probe_Extension (16#00#) then
         Arch.Debug.Print ("SBI Set_Timer unsupported");
         return;
      end if;
      R := SBIEcall (16#00#, 0, Time_Value);
      if R /= 0 then
         Arch.Debug.Print ("Set_Timer error=" & U64'Image (R));
      end if;
   end Set_Timer;

   -- IPI Extension (EID=0x03)
   procedure Send_IPI (Hart_Mask : U64) is
      R : U64;
   begin
      if not Probe_Extension (16#03#) then
         Arch.Debug.Print ("SBI Send_IPI unsupported");
         return;
      end if;
      R := SBIEcall (16#03#, 0, Hart_Mask);
      if R /= 0 then
         Arch.Debug.Print ("Send_IPI error=" & U64'Image (R));
      end if;
   end Send_IPI;

   procedure Clear_IPI (Hart_Mask : U64) is
      R : U64;
   begin
      if not Probe_Extension (16#03#) then
         Arch.Debug.Print ("SBI Clear_IPI unsupported");
         return;
      end if;
      R := SBIEcall (16#03#, 1, Hart_Mask);
      if R /= 0 then
         Arch.Debug.Print ("Clear_IPI error=" & U64'Image (R));
      end if;
   end Clear_IPI;

   -- Remote Fence Extension (EID=0x52464E43)
   procedure Remote_Fence_I (Mask, Base : U64) is
      R : U64;
   begin
      if not Probe_Extension (16#52464E43#) then
         Arch.Debug.Print ("SBI Remote_Fence_I unsupported");
         return;
      end if;
      R := SBIEcall (16#52464E43#, 0, Mask, Base);
      if R /= 0 then
         Arch.Debug.Print ("Remote_Fence_I error=" & U64'Image (R));
      end if;
   end Remote_Fence_I;

   -- Hart State Management (EID=0x48534D)
   procedure Hart_Start (ID, Addr, Opaque : U64) is
      R : U64;
   begin
      if not Probe_Extension (16#48534D#) then
         Arch.Debug.Print ("SBI Hart_Start unsupported");
         return;
      end if;
      R := SBIEcall (16#48534D#, 0, ID, Addr, Opaque);
      if R /= 0 then
         Arch.Debug.Print ("Hart_Start error=" & U64'Image (R));
      end if;
   end Hart_Start;

   -- System Reset Extension (EID=0x08)
   procedure Shutdown is
   begin
      if not Probe_Extension (16#08#) then
         Arch.Debug.Print ("SBI Shutdown unsupported");
      end if;
      SBIEcall (16#08#, 0);
      Lib.Panic.Hard_Panic ("Shutdown returned");
   end Shutdown;

end Arch.SBI;