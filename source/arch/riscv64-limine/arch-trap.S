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
.type trap_entry, @function

trap_entry:
    addi sp, sp, -FRAME_SIZE            # Allocate trap frame

    sd ra, FRAME_RA(sp)                # Save x1 (ra)
    sd tp, FRAME_TP(sp)                # Save x4 (tp)
    sd t0, FRAME_T0(sp)                # Save x5 (t0)
    sd t1, FRAME_T1(sp)                # Save x6 (t1)
    sd t2, FRAME_T2(sp)                # Save x7 (t2)

    sd a0, FRAME_A0(sp)                # Save x10 (a0)
    sd a1, FRAME_A1(sp)                # Save x11 (a1)
    sd a2, FRAME_A2(sp)                # Save x12 (a2)
    sd a3, FRAME_A3(sp)                # Save x13 (a3)
    sd a4, FRAME_A4(sp)                # Save x14 (a4)
    sd a5, FRAME_A5(sp)                # Save x15 (a5)
    sd a6, FRAME_A6(sp)                # Save x16 (a6)
    sd a7, FRAME_A7(sp)                # Save x17 (a7)

    sd t3, FRAME_T3(sp)                # Save x28 (t3)
    sd t4, FRAME_T4(sp)                # Save x29 (t4)
    sd t5, FRAME_T5(sp)                # Save x30 (t5)
    sd t6, FRAME_T6(sp)                # Save x31 (t6)

    sd sp, FRAME_SP(sp)                # Save x2 (sp)
    sd gp, FRAME_GP(sp)                # Save x3 (gp)
    sd s0, FRAME_S0(sp)                # Save x8 (s0/fp)
    sd s1, FRAME_S1(sp)                # Save x9 (s1)
    sd s2, FRAME_S2(sp)                # Save x18 (s2)
    sd s3, FRAME_S3(sp)                # Save x19 (s3)
    sd s4, FRAME_S4(sp)                # Save x20 (s4)
    sd s5, FRAME_S5(sp)                # Save x21 (s5)
    sd s6, FRAME_S6(sp)                # Save x22 (s6)
    sd s7, FRAME_S7(sp)                # Save x23 (s7)
    sd s8, FRAME_S8(sp)                # Save x24 (s8)
    sd s9, FRAME_S9(sp)                # Save x25 (s9)
    sd s10, FRAME_S10(sp)              # Save x26 (s10)
    sd s11, FRAME_S11(sp)              # Save x27 (s11)

    csrr t0, sepc
    sd t0, FRAME_SEPC(sp)              # Save exception PC
    csrr t0, scause
    sd t0, FRAME_SCAUSE(sp)            # Save trap cause
    csrr t0, stval
    sd t0, FRAME_STVAL(sp)             # Save faulting address
    csrr t0, sstatus
    sd t0, FRAME_SSTATUS(sp)           # Save supervisor status

    ld t1, TCB_FPCTX(tp)
    sd t1, FRAME_FPCTX(sp)             # Save FP_Context pointer

    mv a0, sp                          # Pass Frame pointer
    call Handle_Trap                   # Call kernel trap handler

.global trap_exit
trap_exit:
    ld ra, FRAME_RA(sp)                # Restore x1 (ra)
    ld tp, FRAME_TP(sp)                # Restore x4 (tp)
    ld t0, FRAME_T0(sp)                # Restore x5 (t0)
    ld t1, FRAME_T1(sp)                # Restore x6 (t1)
    ld t2, FRAME_T2(sp)                # Restore x7 (t2)

    ld a0, FRAME_A0(sp)                # Restore x10 (a0)
    ld a1, FRAME_A1(sp)                # Restore x11 (a1)
    ld a2, FRAME_A2(sp)                # Restore x12 (a2)
    ld a3, FRAME_A3(sp)                # Restore x13 (a3)
    ld a4, FRAME_A4(sp)                # Restore x14 (a4)
    ld a5, FRAME_A5(sp)                # Restore x15 (a5)
    ld a6, FRAME_A6(sp)                # Restore x16 (a6)
    ld a7, FRAME_A7(sp)                # Restore x17 (a7)

    ld t3, FRAME_T3(sp)                # Restore x28 (t3)
    ld t4, FRAME_T4(sp)                # Restore x29 (t4)
    ld t5, FRAME_T5(sp)                # Restore x30 (t5)
    ld t6, FRAME_T6(sp)                # Restore x31 (t6)

    ld sp, FRAME_SP(sp)                # Restore x2 (sp)
    ld gp, FRAME_GP(sp)                # Restore x3 (gp)
    ld s0, FRAME_S0(sp)                # Restore x8 (s0/fp)
    ld s1, FRAME_S1(sp)                # Restore x9 (s1)
    ld s2, FRAME_S2(sp)                # Restore x18 (s2)
    ld s3, FRAME_S3(sp)                # Restore x19 (s3)
    ld s4, FRAME_S4(sp)                # Restore x20 (s4)
    ld s5, FRAME_S5(sp)                # Restore x21 (s5)
    ld s6, FRAME_S6(sp)                # Restore x22 (s6)
    ld s7, FRAME_S7(sp)                # Restore x23 (s7)
    ld s8, FRAME_S8(sp)                # Restore x24 (s8)
    ld s9, FRAME_S9(sp)                # Restore x25 (s9)
    ld s10, FRAME_S10(sp)              # Restore x26 (s10)
    ld s11, FRAME_S11(sp)              # Restore x27 (s11)

    addi sp, sp, FRAME_SIZE            # Free trap frame
    sret                               # Return from trap
