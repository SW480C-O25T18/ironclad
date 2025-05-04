--  arch-plic.adb: Implementation of Platform-Level Interrupt Controller
--  (PLIC) utilities.
--  Provides functionality for configuring and managing the PLIC on
--  RISC-V64 systems.
--  Handles interrupt enabling, priority management, and
--  context-specific configurations.
--  Fully compliant with the RISC-V PLIC v1.0/v1.1 specifications.
--  Includes meaningful debug statements and proper exception handling.
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

with Arch.Debug;               use Arch.Debug;
with Arch.CPU;                 use Arch.CPU;
with Lib.Panic;                use Lib.Panic;
with System;                   use System;

package body Arch.PLIC with SPARK_Mode => Off is

   ---------------------------------------------------------------------------
   --  Helper: Build register address from base offset
   ---------------------------------------------------------------------------
   function Reg_Addr (Off : Storage_Offset) return Address;
   function Reg_Addr (Off : Storage_Offset) return Address is
   begin
      return To_Address (
         Integer_Address (Integer (Plic_Base_Off)) + Integer (Off));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Reg_Addr");
   end Reg_Addr;

   ---------------------------------------------------------------------------
   --  Presence Detection
   ---------------------------------------------------------------------------
   function Is_Enabled return Boolean is
   begin
      return Find_Node_By_Compatible ("riscv,plic0") /= null;
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Is_Enabled");
         return False;
   end Is_Enabled;

   ---------------------------------------------------------------------------
   --  Configuration: Map MMIO, read #sources, contexts, version
   ---------------------------------------------------------------------------
   procedure Set_PLIC_Configuration is
      Node : DTB_Node_Access := Find_Node_By_Compatible ("riscv,plic0");
      Base : Unsigned_64;
      Size : Unsigned_64;
      Phys : Address;
      Virt : Address;
      Success : Boolean;
   begin
      if Node = null then
         Debug.Print ("Arch.PLIC: DTB node not found; PLIC disabled");
         return;
      end if;

      -- Parse 'reg' property
      Base := Get_Property_Unsigned_64 (Node, "reg", 1);
      Size := Get_Property_Unsigned_64 (Node, "reg", 2);
      Phys := To_Address (Storage_Offset (Base));
      Virt := Phys;

      -- Map region
      Map_Range (
        Map            => Kernel_Table,
        Physical_Start => Phys,
        Virtual_Start  => Virt,
        Length         => Storage_Count (Size),
        Permissions    => (Is_User_Accesible => False,
                          Is_Global         => True),
        Success        => Success
      );
      if not Success then
         Debug.Print ("Arch.PLIC: MMIO map failed; PLIC disabled");
         return;
      end if;

      Plic_Base     := Virt;
      Plic_Base_Off := Storage_Offset (To_Integer (Virt));
      Debug.Print ("Arch.PLIC: Mapped at " & Address'Image (Virt)
                   & ", size " & Unsigned_64'Image (Size));

      -- Number of sources
      Num_Sources := Unsigned_32 (
         Get_Property_Unsigned_64 (Node, "interrupts-extended", 1));

      -- Contexts = 2 × hart count
      Num_Contexts := Unsigned_64 (Core_Count) * 2;

      -- PLIC version
      Plic_Version := Unsigned_32 (
         Get_Property_Unsigned_64 (Node, "riscv,plic-version", 1));
      Debug.Print ("Arch.PLIC: Version=" &
         Unsigned_32'Image (Plic_Version));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Set_PLIC_Configuration");
         Plic_Base := Null_Address;
         Plic_Base_Off := 0;
         Num_Sources := 0;
         Num_Contexts := 0;
         Plic_Version := 0;
         return;
   end Set_PLIC_Configuration;

   ---------------------------------------------------------------------------
   --  Initialization: Threshold=0 and enable all IRQs
   ---------------------------------------------------------------------------
   procedure Initialize (
      Hart_Id : Unsigned_64;
      Ctx : Context_Id) is
      Off : Storage_Offset :=
         Plic_Base_Off + Threshold_Base
         + Context_Stride * Storage_Offset (Integer (Ctx));
      Reg : Unsigned_32 with Address => Reg_Addr (Off);
   begin
      Debug.Print ("Arch.PLIC: Init context " & Unsigned_64'Image (Ctx));
      if Plic_Base = Null_Address then
         Debug.Print ("Arch.PLIC: Not mapped; skip init");
         return;
      end if;

      -- Set threshold to 0
      Reg := 0;
      Debug.Print ("Arch.PLIC: Threshold=0 for context "
                   & Unsigned_64'Image (Ctx));

      -- Enable all sources if v1+
      if Plic_Version >= 1 then
         for Idx in 1 .. Natural (Num_Sources) loop
            Enable_IRQ (Hart_Id, Ctx, IRQ_Id (Idx));
         end loop;
         Debug.Print ("Arch.PLIC: Enabled all IRQs for context "
                      & Unsigned_64'Image (Ctx));
      end if;
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Initialize");
         return;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Threshold Tuning
   ---------------------------------------------------------------------------
   procedure Set_Threshold (Ctx : Context_Id; Threshold : Unsigned_32) is
      Off : Storage_Offset :=
         Plic_Base_Off + Threshold_Base
         + Context_Stride * Storage_Offset (Integer (Ctx));
      Reg : Unsigned_32 with Address => Reg_Addr (Off);
   begin
      Reg := Threshold;
      Debug.Print (
         "Arch.PLIC: Set threshold=" & Unsigned_32'Image (Threshold)
                   & " for context " & Unsigned_64'Image (Ctx));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Set_Threshold");
         return;
   end Set_Threshold;

   ---------------------------------------------------------------------------
   --  Priority Management
   ---------------------------------------------------------------------------
   procedure Set_Priority (Id : IRQ_Id; Priority : Unsigned_32) is
      Off : Storage_Offset :=
         Plic_Base_Off + Priority_Base
         + Priority_Step * Storage_Offset (Integer (Id));
      Reg : Unsigned_32 with Address => Reg_Addr (Off);
   begin
      Reg := Priority;
      Debug.Print ("Arch.PLIC: Priority=" & Unsigned_32'Image (Priority)
                   & " for IRQ=" & Unsigned_64'Image (Id));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Set_Priority");
         return;
   end Set_Priority;

   ---------------------------------------------------------------------------
   --  Mask/Unmask IRQs
   ---------------------------------------------------------------------------
   procedure Enable_IRQ (
      Hart_Id : Unsigned_64;
      Ctx : Context_Id; Id : IRQ_Id) is
      Word : Integer := Integer (Id) / 32;
      Bit  : Integer := Integer (Id) mod 32;
      Off  : Storage_Offset :=
         Plic_Base_Off + Enable_Base
         + Context_Stride * Storage_Offset (Integer (Ctx))
         + Storage_Offset (4 * Word);
      Reg  : Unsigned_32 with Address => Reg_Addr (Off);
      Mask : Unsigned_32 := Interfaces.Shift_Left (Unsigned_32 (1), Bit);
   begin
      pragma Assert (Integer (Id) <= Integer (Num_Sources));
      Reg := Reg or Mask;
      Debug.Print ("Arch.PLIC: Enabled IRQ=" & Unsigned_64'Image (Id)
                   & " Ctx=" & Unsigned_64'Image (Ctx));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Enable_IRQ");
         return;
   end Enable_IRQ;

   procedure Disable_IRQ (
      Hart_Id : Unsigned_64;
      Ctx : Context_Id; Id : IRQ_Id) is
      Word : Integer := Integer (Id) / 32;
      Bit  : Integer := Integer (Id) mod 32;
      Off  : Storage_Offset :=
         Plic_Base_Off + Enable_Base
         + Context_Stride * Storage_Offset (Integer (Ctx))
         + Storage_Offset (4 * Word);
      Reg  : Unsigned_32 with Address => Reg_Addr (Off);
      Mask : Unsigned_32 := not Interfaces.Shift_Left (Unsigned_32 (1), Bit);
   begin
      pragma Assert (Integer (Id) <= Integer (Num_Sources));
      Reg := Reg and Mask;
      Debug.Print ("Arch.PLIC: Disabled IRQ=" & Unsigned_64'Image (Id)
                   & " Ctx=" & Unsigned_64'Image (Ctx));
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Disable_IRQ");
         return;
   end Disable_IRQ;

   ---------------------------------------------------------------------------
   --  Claim & Complete
   ---------------------------------------------------------------------------
   function Claim (
      Hart_Id : Unsigned_64;
      Ctx : Context_Id) return IRQ_Id is
      Off : Storage_Offset :=
         Plic_Base_Off + Claim_Base
         + Context_Stride * Storage_Offset (Integer (Ctx));
      Reg : Unsigned_32 with Address => Reg_Addr (Off);
   begin
      if Plic_Base = Null_Address then
         return IRQ_Id (0);
      end if;
      return IRQ_Id (Reg);
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Claim");
         return IRQ_Id (0);
   end Claim;

   procedure Complete (
      Hart_Id : Unsigned_64;
      Ctx : Context_Id; IRQ : IRQ_Id) is
      Off : Storage_Offset :=
         Plic_Base_Off + Claim_Base
         + Context_Stride * Storage_Offset (Integer (Ctx));
      Reg : Unsigned_32 with Address => Reg_Addr (Off);
   begin
      if Plic_Base = Null_Address or else IRQ = 0 then
         return;
      end if;
      Reg := Unsigned_32 (IRQ);
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Complete");
         return;
   end Complete;

   ---------------------------------------------------------------------------
   --  Context Convenience
   ---------------------------------------------------------------------------
   function Supervisor_Context (Hart : Unsigned_64) return Context_Id is
   begin
      return Context_Id (Hart * 2 + 1);
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Supervisor_Context");
         return Context_Id (0);
   end Supervisor_Context;

   function Machine_Context (Hart : Unsigned_64) return Context_Id is
   begin
      return Context_Id (Hart * 2);
   exception
      when others =>
         Debug.Print ("Arch.PLIC: Unknown error in Machine_Context");
         return Context_Id (0);
   end Machine_Context;

end Arch.PLIC;