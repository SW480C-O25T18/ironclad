--  arch-snippets.ads: Architecture-specific bits.
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

with System.Machine_Code; use System.Machine_Code;
with Arch.Debug;

package body Arch.Snippets is
   procedure HCF is
   begin
      loop
         Debug.Print ("HCF: Halt and Catch Fire");
         Debug.Print ("HCF: Disabling interrupts");
         Disable_Interrupts;
         Debug.Print ("HCF: Waiting for interrupt");
         Wait_For_Interrupt;
      end loop;
   end HCF;

   procedure Enable_Interrupts is
   begin
      Debug.Print ("Enable_Interrupts: Enabling interrupts");
      Write_SStatus (Read_SStatus or 2#10#);
      Debug.Print ("Enable_Interrupts: Interrupts enabled");
   end Enable_Interrupts;

   procedure Disable_Interrupts is
   begin
      Debug.Print ("Disable_Interrupts: Disabling interrupts");
      Write_SStatus (Read_SStatus and not 2#10#);
      Debug.Print ("Disable_Interrupts: Interrupts disabled");
   end Disable_Interrupts;

   procedure Wait_For_Interrupt is
   begin
      Debug.Print ("Wait_For_Interrupt: Waiting for interrupt");
      Asm ("wfi", Volatile => True);
      Debug.Print ("Wait_For_Interrupt: Interrupt received");
   end Wait_For_Interrupt;

   function Interrupts_Enabled return Boolean is
   begin
      Debug.Print ("Interrupts_Enabled: Checking if interrupts are enabled");
      return False;
   end Interrupts_Enabled;

   procedure Pause is
   begin
      Debug.Print ("Pause: No pause equivalent sadly, inneficient busy loops for you!");
      null; --  No pause equivalent sadly, inneficient busy loops for you!
   end Pause;

   function Read_SStatus return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Debug.Print ("Read_SStatus: Reading SStatus");
      Asm ("csrr %0, sstatus",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      Debug.Print ("Read_SStatus: SStatus = " & Unsigned_64'Image(Value));
      Debug.Print ("Read_SStatus: SStatus read");
      return Value;
   end Read_SStatus;

   procedure Write_SStatus (Value : Unsigned_64) is
   begin
      Debug.Print ("Write_SStatus: SStatus = " & Unsigned_64'Image(Value));
      Asm ("csrw sstatus, %0",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
      Debug.Print ("Write_SStatus: SStatus written");
   end Write_SStatus;

      function Read_SATP return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ( --"mov %%satp, %0",
            "csrr %0, satp",
           Outputs  => Unsigned_64'Asm_Output ("=r", Value),
           Clobber  => "memory",
           Volatile => True);
      return Value;
   end Read_SATP;

   procedure Write_SATP (Value : Unsigned_64) is
   begin
      Asm ( -- "mov %0, %%satp",
            "csrw satp, %0",
           Inputs   => Unsigned_64'Asm_Input ("r", Value),
           Clobber  => "memory",
           Volatile => True);
   end Write_SATP;
end Arch.Snippets;
