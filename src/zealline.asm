; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>
;
; SPDX-License-Identifier: Apache-2.0

        INCLUDE "zos_sys.asm"
        INCLUDE "zos_video.asm"
        INCLUDE "zos_keyboard.asm"    

        SECTION TEXT

        ; ---------------------------------------------------------------------
        ; PUBLIC INTERFACE
        ; ---------------------------------------------------------------------
        ; TODO PUBLIC zealline_set_prompt
        ; TODO PUBLIC zealline_print_history
        ; TODO PUBLIC zealline_add_history
        PUBLIC zealline_get_line
        ; ---------------------------------------------------------------------

        ; can not be larger then 255 (see the C register of zline_get_line)
        DEFC MAX_LINE_LENGTH = 80
        DEFC READBUFFER_SIZE = 2

        DEFC BG_COLOR       = TEXT_COLOR_BLACK
        DEFC CURDIR_COLOR   = TEXT_COLOR_LIGHT_GRAY
        DEFC TEXT_COLOR     = TEXT_COLOR_WHITE

        ; kb_flags constants
        DEFC KB_FLAG_NOT_YET_USED_BIT = 7
        DEFC KB_FLAG_CAPSLOCK_BIT     = 6
        DEFC KB_FLAG_RIGHT_CTRL_BIT   = 5
        DEFC KB_FLAG_LEFT_CTRL_BIT    = 4
        DEFC KB_FLAG_RIGHT_ALT_BIT    = 3
        DEFC KB_FLAG_LEFT_ALT_BIT     = 2
        DEFC KB_FLAG_RIGHT_SHIFT_BIT  = 1
        DEFC KB_FLAG_LEFT_SHIFT_BIT   = 0
        DEFC KB_FLAG_ANY_UPPERCASE    = (1 << KB_FLAG_LEFT_SHIFT_BIT) + (1 << KB_FLAG_RIGHT_SHIFT_BIT) + (1 << KB_FLAG_CAPSLOCK_BIT)
        DEFC KB_FLAG_ANY_CTRL         = (1 << KB_FLAG_LEFT_CTRL_BIT) + (1 << KB_FLAG_RIGHT_CTRL_BIT)

        EXTERN OutputRegisters
        EXTERN OutputMemoryAtDE
        EXTERN OutputNewline
        EXTERN zealline_to_uppercase

        ; Stores the Position of the cursor to RAM
        ; Alters: DE, HL, C, A
        MACRO SAVE_CURSOR_POS _
                ld de, cursor_position
                ld h, DEV_STDOUT
                ld c, CMD_GET_CURSOR_XY
                IOCTL()                  ; And Register L
        ENDM

        ; Loads the Position of the cursor to DE
        ; Alters A
        ; Returns:
        ;       DE - d=x-coordinate e=y-coordinate
        MACRO LOAD_CURSOR_SWAPPED _
                ld de, (cursor_position)        ; load cursor position
                ld a, e                         ; and swap
                ld e, d
                ld d, a
        ENDM

        ; Moves the cursor on the screen to the position in DE
        ; Alters: HL, BC
        MACRO MOVE_CURSOR_POS _
                ld h, DEV_STDOUT
                ld c, CMD_SET_CURSOR_XY
                IOCTL()
        ENDM

        MACRO SET_STDIN_TO_RAW__ON_ERROR on_error_lable
                ld h, DEV_STDIN
                ld c, KB_CMD_SET_MODE
                ld e, KB_MODE_RAW
                IOCTL()
                or a
                jp nz, on_error_lable
        ENDM

        ; Saves the screen area at memory location 'screen_area'
        ; Alters: A, C, DE, H
        MACRO GET_SCREEN_AREA__ON_ERROR on_error_lable
                ld de, screen_area
                ld h, DEV_STDOUT
                ld c, CMD_GET_AREA
                IOCTL()
                or a
                jp nz, on_error_lable
        ENDM

        MACRO SET_TEXT_COLOR color
                ld h, DEV_STDOUT
                ld c, CMD_SET_COLORS
                ld d, BG_COLOR
                ld e, color
                IOCTL()
        ENDM

        MACRO PRINT_PROMPT _
                SET_TEXT_COLOR(TEXT_COLOR_LIGHT_GRAY)
                ld a, (default_prompt_length) ; set bc to (default_prompt_length)
                ld b, 0
                ld c, a
                S_WRITE2(DEV_STDOUT, default_prompt); bc == length
                or a
                jp nz, fatal_error_loop
                SET_TEXT_COLOR(TEXT_COLOR_WHITE)
        ENDM

        MACRO ON_IGNORED_SCANCODES_GOTO lable
                cp 0x20                  ; non printable character: 0x20 <= char
                jr c, lable
                cp 0x80                  ; non printable character: char >= 0x80
                jr nc, lable
        ENDM

        MACRO ON_KEY_PUSHED_EVENT_GOTO lable
                cp KB_RELEASED
                jr nz, lable
        ENDM

        ; setup linebuffer for new input
        ; Alters: A
        MACRO INITIALIZE_LINEBUFFER_VARIABLES _
                or a  ; set a to 0
                ld (linebuffer_offset), a
                ld (linebuffer_size), a
        ENDM

        MACRO ON_KEYEVENT_GOTO key, lable
                cp key
                jp z, lable
        ENDM

        MACRO TOGGLE_KB_FLAG flag
                ld a, (kb_flags)
                xor 1 << flag
                ld (kb_flags), a
        ENDM

        ; checks if ctrl key is currently pressed
        ; if so we goto lable in order to handle special key combinations
        ; Alters: IX
        MACRO ON_CTRL_MODE_GOTO lable
                ld ixl, a               ; A contains the entered character, preserve it!
                ld a, (kb_flags)
                and KB_FLAG_ANY_CTRL
                ld a, ixl
                jp nz, lable
        ENDM

        ; GET_LINEBUFFER_AT_OFFSET
        ; Alters: A, DE
        ; Returns:
        ;       HL - position in linebuffer
        MACRO GET_LINEBUFFER_AT_OFFSET _
                ld hl, linebuffer
                ld a, (linebuffer_offset)
                ld d, 0
                ld e, a
                add hl, de
        ENDM

        ; GET_LINEBUFFER_AT_LASTCHAR
        ; Alters: A, DE
        ; Returns:
        ;       HL - position in linebuffer
        MACRO GET_LINEBUFFER_AT_LASTCHAR _
                ld hl, linebuffer
                ld a, (linebuffer_size)
                dec a
                ld d, 0
                ld e, a
                add hl, de
        ENDM

        ; Moves Virtual Position one to the left
        ; Returns:
        ;       A - value in linebuffer_offset
        MACRO DEC_LINEBUFFER_OFFSET _
                ld a, (linebuffer_offset)
                dec a
                ld (linebuffer_offset), a
        ENDM

        ; Moves Virtual Position one to the right
        ; Returns:
        ;       A - value in linebuffer_offset
        MACRO INC_LINEBUFFER_OFFSET _
                ld a, (linebuffer_offset)
                inc a
                ld (linebuffer_offset), a
        ENDM

        ; decrease linebuffer_size by one
        ; Returns:
        ;       A - value in linebuffer_size
        MACRO DEC_LINEBUFFER_SIZE _
                ld a, (linebuffer_size)
                dec a
                ld (linebuffer_size), a
        ENDM

        ; increase linebuffer_size by one
        ; Returns:
        ;       A - value in linebuffer_size
        MACRO INC_LINEBUFFER_SIZE _
                ld a, (linebuffer_size)
                inc a
                ld (linebuffer_size), a
        ENDM

        MACRO HANDLE_VISIBLE_SCANCODES _
                ;Boundary Check
                ld b, a                             ; save Register A in B, B is now the entered Scancode
                ld a, (linebuffer_size)
                inc a                               ; increase the linebuffer size by one!
                cp MAX_LINE_LENGTH
                jp z, _handle_new_input             ; !! handle next char, maybe a delete instruction!
                                                    ; !! because the linebuffer is full - no appending!
                SAVE_CURSOR_POS()                   ; get cursor position
                ; Uppercase Test
                ld a, (kb_flags)
                and KB_FLAG_ANY_UPPERCASE           ; Check if any shift or capslock was set
                jr z, __no_upper_case
                        ; Convert to Uppercase
                        call zealline_to_uppercase      ; converts char in register B into uppercase in A
                        or a
                        jp z, _handle_new_input         ; a holds a nullbyte if there is no uppercase available
                        ld b, a                         ; load uppercase char into into B
        __no_upper_case:
                ld a, (linebuffer_offset)
                ld c, a
                ld a, (linebuffer_size)
                sub c                                   ; a(number of characters to copy) = a(linebuffer_size) - (c)linebuffer_offset
                ld c, a                                 ; set C to number_of_characters_to_copy
                ld ix, bc                               ; STORE somewhere else ixh/b=character_to_append, ixl/c=number_of_characters_to_copy
                jp z, __copy_add_character_completed
                        ; create room in the linebuffer for the newly entered character
                        GET_LINEBUFFER_AT_LASTCHAR()    ; alters registers A and DE
                        ld de, hl
                        inc de                          ; de is end_of_linebuffer + 1
                        ld b, 0                         ; bc == number of chars to copy
                        lddr                            ; copy all bytes!
        __copy_add_character_completed:
                GET_LINEBUFFER_AT_OFFSET()              ; alters registers A and DE
                ; char needs to be placed at (HL)
                ; so make space for it
                ld a, ixh
                ld (hl), a                              ; write the char in A to the linebuffer
                ld (charbuffer), a                      ; write the char in A to the charbuffer
                S_WRITE3(DEV_STDOUT, charbuffer, 1)     ; output it to the screen / Alters: BC, DE, HL
                INC_LINEBUFFER_SIZE()
                INC_LINEBUFFER_OFFSET()
                ld b, a                                 ; b := linebuffer_offset
                ld a, (linebuffer_size)
                sub b                                   ; a := linebuffer_size - linebuffer_offset => number of relocated characters
                jp z, _handle_new_input                 ; if linebuffer_offset == linebuffer_size
                                                        ; then no copied characters needs to be printed out
                ld c, a
                ld b, 0                                 ; bc == length to print
                GET_LINEBUFFER_AT_OFFSET()              ; alters registers A and DE
                ld de, hl                               ; we can't use S_WRITE2(DEV_STDOUT, hl) because it alters HL before setting DE to HL
                S_WRITE1(DEV_STDOUT)                    ; S_WRITE1 uses BC as lenght
                call move_cursor_to_the_right
        ENDM

        MACRO HANDLE_BACKSPACE_EVENT _
                SAVE_CURSOR_POS()         ; get cursor position
                ld a, (linebuffer_offset) ; load x position of VirtualCursor to A
                or a
                jp z, _handle_new_input   ; CASE 1: VirtualCursor was competly left - ignore this
                ; Register A is now the position in the linebuffer
                ld c, a                 ; save value to C
                ld a, (linebuffer_size)
                sub c ; if zeroflag is set we're deleting from the end
                jr z, __backspace_at_the_end ; <= Case 3
        __backspace_somewhere_in_the_middle: ; <= Case 2
                ld b, 0
                ld c, a                         ; bc == a := length of bytes to copy
                GET_LINEBUFFER_AT_OFFSET()      ; alters registers A and DE
                ld de, hl
                dec de                          ; de = linebuffer_at_offset - 1
                ld ix, bc                       ; store bc in IX for later!
                ld iy, de                       ; store DE in IY for later!
                ldir                            ; de (dest), hl (src), bc (byte counter) MOVE all characters one step to the left
                call move_cursor_to_the_left
                S_WRITE3(DEV_STDOUT, iy, ix)             ; output the moved characters
                S_WRITE3(DEV_STDOUT, whitespace_char, 1) ; overwrite the char that was deleted
                call move_cursor_to_the_left
                DEC_LINEBUFFER_OFFSET()
                DEC_LINEBUFFER_SIZE()
                jp _handle_new_input
        __backspace_at_the_end:  ; CASE 3: curser was at the end of the line
                call move_cursor_to_the_left
                S_WRITE3(DEV_STDOUT, whitespace_char, 1) ; overwrite the deleted char with " "
                call move_cursor_to_the_left             ; curser to the new position
                DEC_LINEBUFFER_OFFSET()
                DEC_LINEBUFFER_SIZE()
        ENDM

        ; "zline_get_line" reads a line/command from STDIN
        ;   This is the main function of this library. It will print your
        ;   prompt and read the line into your buffer.
        ;   While MAX_LINE_LENGTH is your line buffer size it still does only
        ;   return the first C bytes.
        ;   The string written to buffer will the null terminated. Keep in mind
        ;   that the null-byte consumeds one byte so there will only be C-1 characters
        ;   in DE. 
        ;   TODO:
        ;     - manage a history
        ;     - call tab completions functions
        ; Parameters:
        ;   DE - Buffer to store the bytes read from the opened device.
        ;   C - Size of the buffer passed, can not be longer then MAX_LINE_LENGTH
        ; Returns:
        ;   A  - ERR_SUCCESS on success, error value else
        ;   BC - Number of bytes filled in DE.
        ; Alters:
        ;   A, BC, DE
zealline_get_line:
        ld b, 0  ; set B to = 0 because we only accept C as length parameter
        dec c    ; decrease c because we need one byte for the 0-byte
        push de  ; save registers for later
        push bc
        ; check if C is within boundaries: must be not largher then MAX
        ld a, c
        cp MAX_LINE_LENGTH
        jp nc, print_requested_to_much_buffer_and_return
        SET_STDIN_TO_RAW__ON_ERROR(print_stdin_raw_error_and_fatalloop)
        GET_SCREEN_AREA__ON_ERROR(print_screen_area_error_and_fatalloop)
_print_prompt:
        PRINT_PROMPT()
        ; STORE_CURSOR_POS__ON_ERROR(print_cursor_read_error_and_fatalloop)
        ; SO WHAT TO DO?
        ; - handle curser left and right
        ; - handle append in the middle
        ; - handle backspace and delete in the middle
        ; - handle STRG+<key>
        ;   * STRG+a = linestart
        ;   * STRG+e = lineend
        ;   * STRG+k = cut fro curser to end
        ; - FUTURE TASK (history) 
        ;   * STRG+r search in history.
        ;   * key up and key down (browse in history)

        INITIALIZE_LINEBUFFER_VARIABLES()
_handle_new_input:
        S_READ3(DEV_STDIN, readbuffer, READBUFFER_SIZE) ; sets DE to readbuffer
        ld a, (de)                 ; read first char into a
        ON_KEY_PUSHED_EVENT_GOTO(_handle_key_pushed_events)
_handle_key_released_events:
                inc de
                ld a, (de) ; read released char with the next scancode
                ON_KEYEVENT_GOTO( KB_LEFT_SHIFT,  _handle_shift_left)
                ON_KEYEVENT_GOTO( KB_RIGHT_SHIFT, _handle_shift_right)
                ON_KEYEVENT_GOTO( KB_LEFT_CTRL,   _handle_ctrl_left)
                ON_KEYEVENT_GOTO( KB_RIGHT_CTRL,  _handle_ctrl_right)
                ON_KEYEVENT_GOTO( KB_LEFT_ALT,    _handle_alt_left)
                ON_KEYEVENT_GOTO( KB_RIGHT_ALT,   _handle_alt_right)
        jr _handle_new_input
_handle_key_pushed_events:
        ON_KEYEVENT_GOTO( KB_LEFT_SHIFT,    _handle_shift_left)
        ON_KEYEVENT_GOTO( KB_RIGHT_SHIFT,   _handle_shift_right)
        ON_KEYEVENT_GOTO( KB_LEFT_CTRL,     _handle_ctrl_left)
        ON_KEYEVENT_GOTO( KB_RIGHT_CTRL,    _handle_ctrl_right)
        ON_KEYEVENT_GOTO( KB_LEFT_ALT,      _handle_alt_left)
        ON_KEYEVENT_GOTO( KB_RIGHT_ALT,     _handle_alt_right)
        ON_KEYEVENT_GOTO( KB_CAPS_LOCK,     _handle_capslock )
        ON_KEYEVENT_GOTO( KB_KEY_ENTER,     _handle_enter)
        ON_KEYEVENT_GOTO( KB_KEY_BACKSPACE, _handle_backspace_event)
        ON_KEYEVENT_GOTO( KB_LEFT_ARROW,    _handle_left_arrow)
        ON_KEYEVENT_GOTO( KB_RIGHT_ARROW,   _handle_right_arrow)
        ON_IGNORED_SCANCODES_GOTO(_handle_new_input) 
        ; everything that reaches this code is a normal scancode
        ON_CTRL_MODE_GOTO( _handle_ctrl_mode)
        HANDLE_VISIBLE_SCANCODES()
        jp _handle_new_input       ; read next character
_handle_ctrl_mode:
        ; handle keystrokes in combination with CTRL
        ON_KEYEVENT_GOTO( 'a',              _handle_ctrl_a )
        ON_KEYEVENT_GOTO( 'e',              _handle_ctrl_e )
        ON_KEYEVENT_GOTO( 'c',              _handle_ctrl_c )
        jp _handle_new_input
_handle_ctrl_a:
        ; Move cursor to the far left
        SAVE_CURSOR_POS()
        ld a, (screen_area + area_width_t)
        ld b, a                         ; divisor is the screen_area width
        ld a, (linebuffer_offset)       ; dividend
        call divide_and_modulo          ; B = A / B and C =A % B
        LOAD_CURSOR_SWAPPED()
        ld a, d                         ; apply modulo on x-axis
        sub b
        ld d, a
        ld a, e                         ; apply quotient on y-axis
        sub c
        ld e, a
        MOVE_CURSOR_POS()
        ld a, 0
        ld (linebuffer_offset), a
        jp _handle_new_input
_handle_ctrl_e:
        ; Move cursor to the far right
        SAVE_CURSOR_POS()
        ld a, (screen_area + area_width_t)
        ld b, a                         ; B := divisor is the screen_area width
        ld a, (linebuffer_offset)
        ld c, a
        ld a, (linebuffer_size)
        sub c                           ; A := steps to the right = linebuffer_size - linebuffer_offset
        call divide_and_modulo          ; B = A / B and C =A % B
        LOAD_CURSOR_SWAPPED()
        ld a, d                         ; apply modulo on x-axis
        add b
        ld d, a
        ld a, e                         ; apply quotient on y-axis
        add c
        ld e, a
        MOVE_CURSOR_POS()
        ld a, (linebuffer_size)
        ld (linebuffer_offset), a
        jp _handle_new_input
_handle_ctrl_c:
        ; Abort this command
        S_WRITE3(DEV_STDOUT, newline_char, 1)   ; print newline
        jp _print_prompt                        ; print prompt
_handle_left_arrow:
        SAVE_CURSOR_POS()
        ; boundary check with linebuffer_offset and 0
        ld a, (linebuffer_offset)
        dec a
        jp m, _handle_new_input
        ld (linebuffer_offset), a
        call move_cursor_to_the_left
        jp _handle_new_input
_handle_right_arrow:
        SAVE_CURSOR_POS()
        ; boundary check with linebuffer_offset and linebuffer_length
        ld a, (linebuffer_offset)
        inc a
        ld b, a
        ld a, (linebuffer_size)
        cp b
        jp c, _handle_new_input
        ld a, b
        ld (linebuffer_offset), a
        call move_cursor_to_the_right
        jp _handle_new_input
_handle_shift_left:
        TOGGLE_KB_FLAG(KB_FLAG_LEFT_SHIFT_BIT)
        jp _handle_new_input
_handle_shift_right:
        TOGGLE_KB_FLAG(KB_FLAG_RIGHT_SHIFT_BIT)
        jp _handle_new_input
_handle_ctrl_left:
        TOGGLE_KB_FLAG(KB_FLAG_RIGHT_CTRL_BIT)
        jp _handle_new_input
_handle_ctrl_right:
        TOGGLE_KB_FLAG(KB_FLAG_RIGHT_CTRL_BIT)
        jp _handle_new_input
_handle_alt_left:
        TOGGLE_KB_FLAG(KB_FLAG_RIGHT_ALT_BIT)
        jp _handle_new_input
_handle_alt_right:
        TOGGLE_KB_FLAG(KB_FLAG_RIGHT_ALT_BIT)
        jp _handle_new_input
_handle_capslock:
        TOGGLE_KB_FLAG(KB_FLAG_CAPSLOCK_BIT)
        jp _handle_new_input
_handle_enter:
        ; return the line by copying it to DE and return the zline_get_line
        ; TODO append to history
        pop bc                     ; restore number of bytes to copy
        pop de                     ; restore destination
        ; C contains the max_size/buffer_size that we should use to return line
        ; to the user. But (linebuffer_size) is what we actually have.
        ld a, (linebuffer_size)
        cp c
        jp nc, __use_c_register    ; Use the smaller value: if carry is not set, A > C, use C
        ld c, a                    ; IF NOT: trick __use_c_register to in fact use A
__use_c_register:
        ld b, 0                    ; only use C of BC
        ld a, c                    ; check if C is zero, if so we're done
        or a
        jr z, __copy_line_completed
        ld hl, linebuffer          ; source
        ldir                       ; ldir counts down to BC == 0
        ld (de), 0
        ld c, a                    ; but A still contains the old c value
__copy_line_completed:
        xor a
        ret                        ; returns BC = length, A = 0 (no error)
_handle_backspace_event:
        HANDLE_BACKSPACE_EVENT()
        jp _handle_new_input

        ; ---------------------------------------------------------------------
        ; PRIVATE_FUNCTIONS (all to be call'ed)
        ; ---------------------------------------------------------------------


        ; divide_and_modulo
        ; calculates A / B as well as A % B
        ; Alters: A
        ; Returns:
        ;       B - A % B
        ;       C - A / B
divide_and_modulo:
        ld c, 0
__division:
        sub b
        jr c, __modulo
        inc c
        jr __division
__modulo:
        add b                           ; we did one substraction to much that we have to fix
        ld b, a                         ; store the modulo value in b, while c is the quotient
        ret


        ; move_cursor_to_the_left
        ;   It assumes that your current coursor position is stored in 
        ;   cursor_position. It moves the cursor left, and if there is
        ;   no left it puts the cursor to the end of the line above.
        ; Returns:
        ;   A  - ERR_SUCCESS on success, error value else
        ; Alters:
        ;   A, BC, DE
move_cursor_to_the_left:
        ld b, h                  ; B is unused: store old H
        ld de, (cursor_position) ; load
        ld a, e                  ; and swap
        ld e, d
        ld d, a
        dec d
        jp p, _set_cursor       ; as soon as we have carry we need to switch lines
                dec e            ; move cursor in the line above
                ld a, (screen_area + area_width_t)
                dec a
                ld d, a          ; set cursor in the end of the line
_set_cursor:
        MOVE_CURSOR_POS()
        ld h, b                  ; restore H value
        ret

        ; move_cursor_to_the_right
        ;   It assumes that your current coursor position is stored in 
        ;   cursor_position. It moves the cursor right, and if there is
        ;   no right it puts the cursor to the beginning of the line below.
        ; Returns:
        ;   A  - ERR_SUCCESS on success, error value else
        ; Alters:
        ;   A, BC, DE
move_cursor_to_the_right:
        ld b, h                  ; B is unused: store old H
        ld de, (cursor_position) ; load
        ld a, e                  ; and swap
        ld e, d
        ld d, a
        inc d
        ld a, (screen_area + area_width_t)
        cp d   ; cmp with end of screeen
        jr nz, _set_cursor
                inc e            ; move cursor in the line below
                ld d, 0          ; set cursor in the beginning of the line
        jr _set_cursor


        ; ---------------------------------------------------------------------
        ; ERROR HANDLING ROUTINES
        ; ---------------------------------------------------------------------
print_stdin_raw_error_and_fatalloop:
        S_WRITE3(DEV_STDOUT, _set_stdin_raw_error, _set_stdin_raw_error_end - _set_stdin_raw_error)        
        jp fatal_error_loop

print_screen_area_error_and_fatalloop:
        S_WRITE3(DEV_STDOUT, _screen_area_error, _screen_area_error_end - _screen_area_error)
        jp fatal_error_loop

print_cursor_read_error_and_fatalloop:
        S_WRITE3(DEV_STDOUT, _cursor_read_error, _cursor_read_error_end - _cursor_read_error)        
        jr fatal_error_loop

print_requested_to_much_buffer_and_return:
        S_WRITE3(DEV_STDOUT, _requested_to_much, _requested_to_much_end - _requested_to_much)
        ret

fatal_error_loop: ; just terminate
        halt

        ; ---------------------------------------------------------------------
        SECTION DATA
        ; ---------------------------------------------------------------------

default_prompt:             defm "zealline> "
default_prompt_length:      defs 1, 10           ; TODO we can later allow to modify this prompt
whitespace_char:            defm " "
newline_char:               defm "\n"

_screen_area_error: DEFM "error: cant read the screen area\n"
_screen_area_error_end:
_requested_to_much: DEFM "error: you can not request more bytes then MAX_LINE_LENGTH\n"
_requested_to_much_end:
_cursor_read_error: DEFM "error: cant set stdin mode to KB_MODE_RAW\n"
_cursor_read_error_end:
_set_stdin_raw_error: DEFM "error: cant set stdin mode to KB_MODE_RAW\n"
_set_stdin_raw_error_end:

        ; ---------------------------------------------------------------------
        SECTION BSS
        ; ---------------------------------------------------------------------

readbuffer:                 defs 2
charbuffer:                 defs 1
linebuffer:                 defs MAX_LINE_LENGTH + 1 ; line + newline_char
linebuffer_offset:          defs 1
linebuffer_size:            defs 1
kb_flags:                   defs 1 ; store shift and caps lock, etc
cursor_position:            defs 2 ; x: Low Byte // y: High Byte
screen_area:                defs 4 ; area_t
