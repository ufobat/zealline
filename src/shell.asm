; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    org 0x4000

    EXTERN OutputNewline
    EXTERN OutputRegisters
    EXTERN OutputMemoryAtDE
    EXTERN zealline_get_line
    EXTERN zealline_set_prompt
    EXTERN zealline_init

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
    ON_STR_STARTSWITH(command, prompt_command, set_prompt)
    jp loop
set_prompt:
    ; command starts with "sp "<new_prompt>
    ; call zealline_set_prompt
    ld hl, command
    add hl, 3
    ld de, hl
    call OutputMemoryAtDE
    call zealline_set_prompt
    jp loop

exit_program:
    ld h, 0
    EXIT();

        ; Checks if the NULL-terminated string in DE is the beginning of the
        ; NULL-terminated string in HL.
        ; Parameters:
        ;   HL - First NULL-terminated string
        ;   DE - Second NULL-terminated string
        ; Returns:
        ;   A - 0 AND Z-Flag, if DE is the beginning
        ;       Negative value if HL > DE
        ;       Positive value if HL < DE
        ; Alters:
        ;   A
str_startswith:
        push hl
        push de
        dec hl
        dec de
_str_startswith_loop:
        inc hl
        inc de
        ld a, (de)
        or a                        ; Check if DE has reached the end
        jr z, _str_startswith_end
        sub (hl)
        jr z, _str_startswith_loop
_str_startswith_end:
        pop de
        pop hl
        ret


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
prompt_command: defb "sp ", 0

    SECTION BCC
command: defs 100, 0
prompt_buffer: defs PATH_MAX, 0

