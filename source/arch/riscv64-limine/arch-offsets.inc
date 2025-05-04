#  arch-offsets.inc: RISC-V64 Trap Frame & TCB Offsets
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

.set FRAME_RA,        0     # x1_ra
.set FRAME_TP,        8     # x4_tp
.set FRAME_T0,       16     # x5_t0
.set FRAME_T1,       24     # x6_t1
.set FRAME_T2,       32     # x7_t2

.set FRAME_A0,       40     # x10_a0
.set FRAME_A1,       48     # x11_a1
.set FRAME_A2,       56     # x12_a2
.set FRAME_A3,       64     # x13_a3
.set FRAME_A4,       72     # x14_a4
.set FRAME_A5,       80     # x15_a5
.set FRAME_A6,       88     # x16_a6
.set FRAME_A7,       96     # x17_a7

.set FRAME_T3,      104     # x28_t3
.set FRAME_T4,      112     # x29_t4
.set FRAME_T5,      120     # x30_t5
.set FRAME_T6,      128     # x31_t6

.set FRAME_SP,      136     # x2_sp
.set FRAME_GP,      144     # x3_gp
.set FRAME_S0,      152     # x8_s0
.set FRAME_S1,      160     # x9_s1
.set FRAME_S2,      168     # x18_s2
.set FRAME_S3,      176     # x19_s3
.set FRAME_S4,      184     # x20_s4
.set FRAME_S5,      192     # x21_s5
.set FRAME_S6,      200     # x22_s6
.set FRAME_S7,      208     # x23_s7
.set FRAME_S8,      216     # x24_s8
.set FRAME_S9,      224     # x25_s9
.set FRAME_S10,     232     # x26_s10
.set FRAME_S11,     240     # x27_s11

.set FRAME_SEPC,    248     # sepc
.set FRAME_SCAUSE,  256     # scause
.set FRAME_STVAL,   264     # stval
.set FRAME_SSTATUS, 272     # sstatus

.set FRAME_FPCTX,   280     # FP_Context_Ptr
.set FRAME_SIZE,    288     # Total size of Frame

#  ----------------------------------------------------------------------------
#  TCB_* macros to access the current-context pointer in the TCB via TP
#  The TCB_CONTEXT_OFFSET in Ada is 72 bytes, so:
.set TCB_FPCTX,      72     # Offset of the context-pointer field in the TCB
