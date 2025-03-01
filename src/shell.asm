; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

    INCLUDE "zos_sys.asm"
    org 0x4000

    EXTERN OutputNewline
    EXTERN OutputRegisters
    EXTERN OutputMemoryAtDE
    EXTERN zealline_get_line

main:
    ; read up to 20 bytes of data
    ld hl, 20
    ld (command_length), hl 
    ; set parameterss for zline_get_line
    ld de, command
    ld bc, (command_length)
    
    call zealline_get_line
    ld (command_length), bc ; store read out data to command_length for the check

    call OutputNewline
    call OutputNewline

    ; output read command
    ld de, command
    ld h, DEV_STDOUT
    WRITE()

    ld de, _debug_command_message
    ld bc, _debug_command_message_end - _debug_command_message
    WRITE()

    ; print it as a memory dump
    ld bc, (command_length)
    ld de, command
    call OutputMemoryAtDE
    call OutputRegisters

_check_for_exit:
    ld a, (command_length) ; ignore the high byte
    cp 4
    jp nz, main

    ld a, (command)
    cp 'e'
    jp nz, main

    ld a, (command+1)
    cp 'x'
    jp nz, main

    ld a, (command+2)
    cp 'i'
    jp nz, main

    ld a, (command+3)
    cp 't'
    jp nz, main

    ld a, 0
    EXIT();

    SECTION DATA

_debug_command_message: DEFM " <- this was my command\n"
_debug_command_message_end:

    SECTION BCC
command: defs 30, 0
command_length: defs 2

