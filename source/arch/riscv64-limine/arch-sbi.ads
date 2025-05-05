--  arch-sbi.ads: Supervisor Binary Interface (SBI) wrapper for riscv64-limine
--  Copyright (C) 2025 Sean Weeks
--
--  This file is part of Ironclad.
--  Distributed under the terms of the GNU GPL v3 or later.

with Interfaces;
with System;
with Arch.Debug;
with Lib.Panic;

package Arch.SBI is
   pragma Pure;

   subtype U64 is Interfaces.Unsigned_64;
   subtype U32 is Interfaces.Unsigned_32;

   ----------------------------------------------------------------
   --  Base Extension (EID = 0x10) :contentReference[oaicite:1]{index=1}
   ----------------------------------------------------------------

   function  Get_Spec_Version return U64;
     -- Returns (major << 24) | (minor << 16) | patch.

   function  Probe_Extension (EID : U64) return Boolean;
     -- Returns True if that extension ID is implemented.

   ----------------------------------------------------------------
   --  Legacy Console Extension (EID = 0x01) 
   ----------------------------------------------------------------

   procedure Console_Putchar (Ch : Character);
     -- sbi_console_putchar; blocks or drops char.

   function  Console_Getchar return Integer;
     -- sbi_console_getchar; returns -1 if no char.

   ----------------------------------------------------------------
   --  Timer Extension (EID = 0x00) :contentReference[oaicite:3]{index=3}
   ----------------------------------------------------------------

   procedure Set_Timer (Time_Value : U64);
     -- sbi_set_timer(Time_Value): schedule next timer IRQ.

   ----------------------------------------------------------------
   --  IPI Extension (EID = 0x03) :contentReference[oaicite:4]{index=4}
   ----------------------------------------------------------------

   procedure Send_IPI  (Hart_Mask : U64);
   procedure Clear_IPI (Hart_Mask : U64);
     -- Send/clear software IPIs.

   ----------------------------------------------------------------
   --  Remote Fence Extension (RFENCE, EID = 0x52464E43) 
   ----------------------------------------------------------------

   procedure Remote_Fence_I    (Hart_Mask, Hart_Mask_Base : U64);
   procedure Remote_Sfence_VMA (Hart_Mask, Hart_Mask_Base : U64;
                               Addr, Size               : U64);
   procedure Remote_Sfence_VMA_ASID
     (Hart_Mask, Hart_Mask_Base : U64;
      Addr, Size, ASID         : U64);
   -- Plus HFENCE.GVMA and HFENCE.VVMA variants per spec.

   ----------------------------------------------------------------
   --  Hart State Management (HSM, EID = 0x48534D) 
   ----------------------------------------------------------------

   procedure Hart_Start   (HartID : U64; Start_Addr, Opaque : U64);
   procedure Hart_Stop    (HartID : U64);
   function  Hart_Get_Status (HartID : U64) return U64;
   procedure Hart_Suspend   (Suspend_Type, Resume_Addr, Opaque : U64);

   ----------------------------------------------------------------
   --  System Reset Extension (EID = 0x08) 
   ----------------------------------------------------------------

   procedure Shutdown;

   ----------------------------------------------------------------
   --  Performance Monitoring Unit (PMU, EID = 0x504D55) 
   ----------------------------------------------------------------

   procedure Pmu_Reset (Counter_ID : U64);
   procedure Pmu_Start (Counter_ID : U64);
   procedure Pmu_Stop  (Counter_ID : U64);
   function  Pmu_Read  (Counter_ID : U64) return U64;

   ----------------------------------------------------------------
   --  System Suspend Extension (EID = 0x53555350) :contentReference[oaicite:9]{index=9}
   ----------------------------------------------------------------

   procedure System_Suspend (Flags : U64);

private
   -- The low-level ECALL wrapper; not callable by clients.
   function SBIEcall
     (ExtID, FunID : U64;
      Arg0, Arg1, Arg2 : U64 := 0) return U64;

end Arch.SBI;