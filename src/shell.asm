; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    org 0x4000

    EXTERN OutputNewline
    EXTERN OutputRegisters
    EXTERN OutputMemoryAtDE
    EXTERN zealline_get_line

    MACRO ON_STREQ str1, str2, label
        ld de, str1
        ld hl, str2
        call strcmp
        or a
        jp z, label
    ENDM

main:
    ; read up to 100 bytes of data
    ; set parameterss for zealline_get_line
    ld de, command
    ld c, 100;
    
    call zealline_get_line

    ld ix, bc       ; store command length
    S_WRITE3(DEV_STDOUT, newline_char, 1)
    S_WRITE3(DEV_STDOUT, command, ix)
    S_WRITE3(DEV_STDOUT, debug_command_message, debug_command_message_end - debug_command_message)

    ON_STREQ(command, exit_command, exit_program)
    jp main

exit_program:
    ld h, 0
    EXIT();


        ; Compare two NULL-terminated strings pointed by HL and DE.
        ; If they are identical, A will be 0
        ; If DE is greater than HL, A will be positive
        ; If HL is greater than DE, A will be negative
        ; Parameters:
        ;   HL - First NULL-terminated string
        ;   DE - Second NULL-terminated string
        ; Returns:
        ;   A - 0 if both are identical
        ;       Negative value if HL > DE
        ;       Positive value if HL < DE
        ; Alters:
        ;   A
        PUBLIC strcmp
strcmp:
        push hl
        push de
        dec hl
        dec de
_strcmp_compare:
        inc hl
        inc de
        ld a, (de)
        sub (hl)
        jr nz, _strcmp_end
        ; Check if both strings have reached the end
        ; If this is the case, or (hl) will reset in zero flag to be set
        ; In that case, no need to continue, we can return, with flag Z set
        or (hl)
        jr nz, _strcmp_compare
_strcmp_end:
        pop de
        pop hl
        ret


    SECTION DATA

debug_command_message: DEFM " <- this was my command\n"
debug_command_message_end:
newline_char: defm "\n"
exit_command: defb "exit", 0

    SECTION BCC
command: defs 100, 0
prompt_buffer: defs PATH_MAX, 0

