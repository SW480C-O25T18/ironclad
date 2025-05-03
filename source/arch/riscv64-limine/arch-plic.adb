--  arch-plic.adb: Implementation of Platform-Level Interrupt Controller
--  (PLIC) utilities.
--  Copyright (C) 2025 Sean C. Weeks - badrock1983
--  Based on RISC-V PLIC v1.0/v1.1 specifications
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

with Interfaces;               use Interfaces;
with System;                   use System;
with System.Storage_Elements;  use System.Storage_Elements;
with Arch.CPU;                 use Arch.CPU;
with Arch.DTB;                 use Arch.DTB;
with Arch.MMU;                 use Arch.MMU;
with Arch.Debug;               use Arch.Debug;
with Lib.Panic;                use Lib.Panic;

package body Arch.PLIC is

   ----------------------------------------------------------------------------
   --  Context Helpers
   ----------------------------------------------------------------------------
   function Supervisor_Context (Hart : Unsigned_64) return Context_Id is
   begin
      return Context_Id (Hart * 2 + 1);
   end Supervisor_Context;

   function Machine_Context (Hart : Unsigned_64) return Context_Id is
   begin
      return Context_Id (Hart * 2);
   end Machine_Context;

   ----------------------------------------------------------------------------
   --  Is_Enabled: True if PLIC DTB node exists
   ----------------------------------------------------------------------------
   function Is_Enabled return Boolean is
      Node : DTB_Node_Access := Find_Node_By_Compatible ("riscv,plic0");
   begin
      return Node /= null;
   end Is_Enabled;

   ----------------------------------------------------------------------------
   --  Global configuration: map MMIO and detect version
   ----------------------------------------------------------------------------
   procedure Set_PLIC_Configuration is
      Node    : DTB_Node_Access := Find_Node_By_Compatible ("riscv,plic0");
      Regs    : Unsigned_64_Array;
      Base    : Unsigned_64;
      Size    : Unsigned_64;
      Phys    : Address;
      Virt    : Address;
      Success : Boolean;
      VerArr  : Unsigned_64_Array;
   begin
      Debug.Print ("Arch.PLIC: Configuring PLIC");
      if Node = null then
         Debug.Print ("Arch.PLIC: DTB node not found; disabled");
         return;
      end if;

      Regs := Get_Property_Unsigned_64 (Node, "reg");
      Base := Regs(1);
      Size := Regs(2);
      Phys := To_Address (Storage_Offset(Base));
      Virt := Phys;

      Map_Range (
        Map            => Kernel_Table,
        Physical_Start => Phys,
        Virtual_Start  => Virt,
        Length         => Storage_Count(Size),
        Permissions    => (Is_User_Accesible => False,
                          Can_Read          => True,
                          Can_Write         => False,
                          Can_Execute       => False,
                          Is_Global         => True),
        Success        => Success
      );
      if not Success then
         Debug.Print ("Arch.PLIC: MMIO map failed; disabled");
         return;
      end if;

      Plic_Base     := Virt;
      Plic_Base_Off := Storage_Offset(To_Integer(Virt));
      Debug.Print ("Arch.PLIC: Mapped at " & Address'Image(Virt)
                   & ", size " & Unsigned_64'Image(Size));

      begin
         Num_Sources := Unsigned_32 (
           Get_Property_Unsigned_64(Node, "#interrupts")(1)
         );
      exception
         when others =>
            Debug.Print ("Arch.PLIC: '#interrupts' missing; default " &
                         Unsigned_32'Image(Num_Sources));
      end;

      Num_Contexts := Core_Count * 2;

      begin
         VerArr := Get_Property_Unsigned_64(Node, "riscv,plic-version");
         Plic_Version := Unsigned_32(VerArr(1));
         Debug.Print ("Arch.PLIC: Version=" & Unsigned_32'Image(Plic_Version));
      exception
         when others =>
            Debug.Print ("Arch.PLIC: Version unspecified; default v1");
      end;

      Debug.Print ("Arch.PLIC: Sources=" & Unsigned_32'Image(Num_Sources)
                 & ", Contexts=" & Unsigned_64'Image(Num_Contexts));
   end Set_PLIC_Configuration;

   ----------------------------------------------------------------------------
   --  Initialize: set threshold and enable IRQs for v1+
   ----------------------------------------------------------------------------
   procedure Initialize (Hart_Id : Unsigned_64; Ctx : Context_Id) is
      Off : Storage_Offset :=
         Plic_Base_Off + Threshold_Base
         + Context_Stride * Storage_Offset(Integer(Ctx));
      Reg : Unsigned_32 with Address => To_Address(Off);
      Idx : Natural;
   begin
      Debug.Print ("Arch.PLIC: Init context " & Unsigned_64'Image(Ctx));
      if Plic_Base = Null_Address then
         Debug.Print ("Arch.PLIC: Not mapped; skip init");
         return;
      end if;

      Reg := 0;

      if Plic_Version >= 1 then
         for Idx in 1 .. Natural(Num_Sources) loop
            Enable_IRQ(Hart_Id, Ctx, IRQ_Id(Idx));
         end loop;
         Debug.Print ("Arch.PLIC: Enabled all IRQs for context " &
                      Unsigned_64'Image(Ctx));
      end if;
   end Initialize;

   ----------------------------------------------------------------------------
   --  Set_Priority: write priority register
   ----------------------------------------------------------------------------
   procedure Set_Priority (Id : IRQ_Id; Priority : Unsigned_32) is
      Off : Storage_Offset :=
         Plic_Base_Off + Priority_Base
         + Storage_Offset(4 * Integer(Id));
      Reg : Unsigned_32 with Address => To_Address(Off);
   begin
      pragma Assert (Integer(Id) < Integer(Num_Sources));
      Reg := Priority;
      Debug.Print ("Arch.PLIC: Prio=" & Unsigned_32'Image(Priority)
                   & " IRQ=" & Unsigned_64'Image(Id));
   end Set_Priority;

   ----------------------------------------------------------------------------
   --  Enable_IRQ: unmask bit in enable register
   ----------------------------------------------------------------------------
   procedure Enable_IRQ (Hart_Id : Unsigned_64;
                         Ctx     : Context_Id;
                         Id      : IRQ_Id) is
      Word : Integer := Integer(Id) / 32;
      Bit  : Integer := Integer(Id) mod 32;
      Off  : Storage_Offset :=
         Plic_Base_Off + Enable_Base
         + Context_Stride * Storage_Offset(Integer(Ctx))
         + Storage_Offset(4 * Word);
      Reg  : Unsigned_32 with Address => To_Address(Off);
      Mask : Unsigned_32 := Interfaces.Shift_Left(Unsigned_32(1), Bit);
   begin
      pragma Assert (Integer(Id) < Integer(Num_Sources));
      Reg := Reg or Mask;
      Debug.Print ("Arch.PLIC: Enabled IRQ=" & Unsigned_64'Image(Id)
                   & " Ctx=" & Unsigned_64'Image(Ctx));
   end Enable_IRQ;

   ----------------------------------------------------------------------------
   --  Disable_IRQ: mask bit in enable register
   ----------------------------------------------------------------------------
   procedure Disable_IRQ (Hart_Id : Unsigned_64;
                          Ctx     : Context_Id;
                          Id      : IRQ_Id) is
      Word : Integer := Integer(Id) / 32;
      Bit  : Integer := Integer(Id) mod 32;
      Off  : Storage_Offset :=
         Plic_Base_Off + Enable_Base
         + Context_Stride * Storage_Offset(Integer(Ctx))
         + Storage_Offset(4 * Word);
      Reg  : Unsigned_32 with Address => To<Address(Off);
      Mask : Unsigned_32 := not Interfaces.Shift_Left(Unsigned_32(1), Bit);
   begin
      pragma Assert (Integer(Id) < Integer(Num_Sources));
      Reg := Reg and Mask;
      Debug.Print ("Arch.PLIC: Disabled IRQ=" & Unsigned_64'Image(Id)
                   & " Ctx=" & Unsigned_64'Image(Ctx));
   end Disable_IRQ;

   ----------------------------------------------------------------------------
   --  Claim: read claim register
   ----------------------------------------------------------------------------
   function Claim (Hart_Id : Unsigned_64;
                   Ctx     : Context_Id) return IRQ_Id is
      Off : Storage_Offset :=
         Plic_Base_Off + Claim_Base
         + Context_Stride * Storage_Offset(Integer(Ctx));
      Reg : Unsigned_32 with Address => To<Address(Off);
      Id  : Unsigned_32;
   begin
      if Plic_Base = Null_Address then
         return IRQ_Id(0);
      end if;
      Id := Reg;
      if Id = 0 then
         return IRQ_Id(0);
      end if;
      Debug.Print ("Arch.PLIC: Claimed IRQ=" & Unsigned_32'Image(Id));
      return IRQ_Id(Id);
   end Claim;

   ----------------------------------------------------------------------------
   --  Complete: write complete register
   ----------------------------------------------------------------------------
   procedure Complete (Hart_Id : Unsigned_64;
                       Ctx     : Context_Id;
                       IRQ     : IRQ_Id) is
      Off : Storage_Offset :=
         Plic_Base_Off + Claim_Base
         + Context_Stride * Storage_Offset(Integer(Ctx));
      Reg : Unsigned_32 with Address => To<Address(Off);
   begin
      if Plic_Base = Null_Address or else IRQ = 0 then
         return;
      end if;
      Reg := Unsigned_32(IRQ);
      Debug.Print ("Arch.PLIC: Completed IRQ=" & Unsigned_64'Image(IRQ));
   end Complete;

end Arch.PLIC;