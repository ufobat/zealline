; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    INCLUDE "zealline.asm"
    INCLUDE "strutils_h.asm"

    org 0x4000

    MACRO ON_STREQ str1, str2, label
        ld hl, str1
        ld de, str2
        call strcmp
        jp z, label
    ENDM

    MACRO ON_STR_STARTSWITH str1, str2, label
        ld hl, str1
        ld de, str2
        call str_startswith
        jp z, label
    ENDM

main:
    call zealline_init
    ; read up to 100 bytes of data
    ; set parameterss for zealline_get_line
loop:
    ld de, command
    ld c, 100
    call zealline_get_line

    ld ix, bc       ; store command length
    S_WRITE3(DEV_STDOUT, newline_char, 1)
    S_WRITE3(DEV_STDOUT, command, ix)
    S_WRITE3(DEV_STDOUT, debug_command_message, debug_command_message_end - debug_command_message)

    ON_STREQ(command, exit_command, exit_program)
    ld hl, command
    call zealline_add_history
    ON_STR_STARTSWITH(command, prompt_command, set_prompt)
    jp loop
set_prompt:
    ; command starts with "sp "<new_prompt>
    ; call zealline_set_prompt
    ld hl, command
    add hl, 3
    ld de, hl
    call zealline_set_prompt
    jp loop

exit_program:
    ld h, 0
    EXIT();

    SECTION DATA

debug_command_message: DEFM " <- this was my command\n"
debug_command_message_end:
newline_char: defm "\n"
exit_command: defb "exit", 0
prompt_command: defb "sp ", 0

    SECTION BCC
command: defs 100, 0
prompt_buffer: defs PATH_MAX, 0

