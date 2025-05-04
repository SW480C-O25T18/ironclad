#  arch-trap.S: RISC-V64 Trap Entry Assembly
#  Copyright (C) 2025 Sean C. Weeks - badrock1983
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

.include "arch-offsets.inc"

.section .text
.global trap_entry
.type   trap_entry, @function

trap_entry:
    addi    sp, sp, -FRAME_SIZE         # Allocate full trap frame

    # --- save caller‑saved GPRs ---
    sd      ra, FRAME_RA(sp)            # x1
    sd      tp, FRAME_TP(sp)            # x4
    sd      t0, FRAME_T0(sp)            # x5
    sd      t1, FRAME_T1(sp)            # x6
    sd      t2, FRAME_T2(sp)            # x7

    sd      a0, FRAME_A0(sp)            # x10
    sd      a1, FRAME_A1(sp)            # x11
    sd      a2, FRAME_A2(sp)            # x12
    sd      a3, FRAME_A3(sp)            # x13
    sd      a4, FRAME_A4(sp)            # x14
    sd      a5, FRAME_A5(sp)            # x15
    sd      a6, FRAME_A6(sp)            # x16
    sd      a7, FRAME_A7(sp)            # x17

    sd      t3, FRAME_T3(sp)            # x28
    sd      t4, FRAME_T4(sp)            # x29
    sd      t5, FRAME_T5(sp)            # x30
    sd      t6, FRAME_T6(sp)            # x31

    # --- save callee‑saved GPRs ---
    sd      sp, FRAME_SP(sp)            # x2
    sd      gp, FRAME_GP(sp)            # x3
    sd      s0, FRAME_S0(sp)            # x8
    sd      s1, FRAME_S1(sp)            # x9
    sd      s2, FRAME_S2(sp)            # x18
    sd      s3, FRAME_S3(sp)            # x19
    sd      s4, FRAME_S4(sp)            # x20
    sd      s5, FRAME_S5(sp)            # x21
    sd      s6, FRAME_S6(sp)            # x22
    sd      s7, FRAME_S7(sp)            # x23
    sd      s8, FRAME_S8(sp)            # x24
    sd      s9, FRAME_S9(sp)            # x25
    sd      s10, FRAME_S10(sp)          # x26
    sd      s11, FRAME_S11(sp)          # x27

    # --- save CSRs ---
    csrr    t0, sepc
    sd      t0, FRAME_SEPC(sp)
    csrr    t0, scause
    sd      t0, FRAME_SCAUSE(sp)
    csrr    t0, stval
    sd      t0, FRAME_STVAL(sp)
    csrr    t0, sstatus
    sd      t0, FRAME_SSTATUS(sp)

    # --- save floating‑point control/status ---
    csrr    t0, fcsr
    sd      t0, FRAME_FCSR(sp)

    # --- save lazy-FP pointer from TCB ---
    ld      t1, TCB_FPCTX(tp)
    sd      t1, FRAME_FPCTX(sp)

    # --- re‑enable interrupts (for nested) ---
    li      t0, 2                   # SIE mask
    csrs    sstatus, t0

    # --- call into Ada trap handler ---
    mv      a0, sp
    call    Handle_Trap

    # (never returns here)

    .global trap_exit
    .type   trap_exit, @function

trap_exit:
    # --- restore caller‑saved GPRs ---
    ld      ra, FRAME_RA(sp)
    ld      tp, FRAME_TP(sp)
    ld      t0, FRAME_T0(sp)
    ld      t1, FRAME_T1(sp)
    ld      t2, FRAME_T2(sp)

    ld      a0, FRAME_A0(sp)
    ld      a1, FRAME_A1(sp)
    ld      a2, FRAME_A2(sp)
    ld      a3, FRAME_A3(sp)
    ld      a4, FRAME_A4(sp)
    ld      a5, FRAME_A5(sp)
    ld      a6, FRAME_A6(sp)
    ld      a7, FRAME_A7(sp)

    ld      t3, FRAME_T3(sp)
    ld      t4, FRAME_T4(sp)
    ld      t5, FRAME_T5(sp)
    ld      t6, FRAME_T6(sp)

    # --- restore callee‑saved GPRs ---
    ld      sp, FRAME_SP(sp)
    ld      gp, FRAME_GP(sp)
    ld      s0, FRAME_S0(sp)
    ld      s1, FRAME_S1(sp)
    ld      s2, FRAME_S2(sp)
    ld      s3, FRAME_S3(sp)
    ld      s4, FRAME_S4(sp)
    ld      s5, FRAME_S5(sp)
    ld      s6, FRAME_S6(sp)
    ld      s7, FRAME_S7(sp)
    ld      s8, FRAME_S8(sp)
    ld      s9, FRAME_S9(sp)
    ld      s10, FRAME_S10(sp)
    ld      s11, FRAME_S11(sp)

    # --- restore supervisor CSRs ---
    ld      t0, FRAME_SSTATUS(sp)
    csrw    sstatus, t0

    ld      t0, FRAME_SEPC(sp)
    csrw    sepc, t0

    # --- restore floating‑point control/status ---
    ld      t0, FRAME_FCSR(sp)
    csrw    fcsr, t0

    # --- tear down stack and return ---
    addi    sp, sp, FRAME_SIZE
    sret
