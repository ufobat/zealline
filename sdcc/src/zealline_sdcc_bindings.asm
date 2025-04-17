; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

    ; The following routines are written with SDCC new calling convention:
    ; __sdcccall(1).
    ; Thus, most parameters will be given in registers. Also, because the
    ; routines don't have variadic arguments and return a value less or equal
    ; to 16-bit, we will need to clean the stack.

    ; int zealline_init();

    .globl _zealline_init
_zealline_init:
    call zealline_init
    ret
