; SPDX-FileCopyrightText: 2025 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

        INCLUDE "zos_sys.asm"
        INCLUDE "zealline_configuration.asm"
        INCLUDE "strutils_h.asm"

        SECTION TEXT

        ; ---------------------------------------------------------------------
        ; PUBLIC INTERFACE
        ; ---------------------------------------------------------------------
        PUBLIC zealline_reset_history_search
        PUBLIC zealline_history_search_backward
        PUBLIC zealline_history_search_forward
        ;PUBLIC zealline_history_search_backward_substr
        ;PUBLIC zealline_history_search_backward_substr
        PUBLIC zealline_add_history

        EXTERN OutputRegisters
        EXTERN OutputMemoryAtDE
        EXTERN OutputNewline


        ; History
        ASSERT(HISTORY_SIZE >= 512)     ; the ring buffer must be at least big enough to store 2 commands
                                        ; just to be super save that deleting (some) entries will always create
                                        ; sufficient space for the next new entry
        DEFVARS 0 {
                history_entry_next      DS.W 1
                history_entry_line_len  DS.B 1
                history_entry_line_ptr  DS.W 1
        }

        ; Performs ADD HL, A
        ; Alters: HL
        MACRO ADD_HL_A _
                add l
                ld l, a
                adc h
                sub l
                ld h, a
        ENDM

        ; Adds the history alignment to Register A
        ;   Increases the value in A till it is a muliple of 4
        ; Alters: A
        MACRO ADD_HISTORY_ALIGNMENT _
                add history_entry_line_ptr      ; add offset for the header, until the string actually begins
                or 3                            ; set the last 2 bits to high and add 1
                add 1                           ; so we end on the address of the next item in a aligned way
        ENDM

        ; Checks if the adress in HL is still before the end of the history ringbuffer
        ;   Jumps to the labels
        ; Alters: A
        MACRO ON_HL_BEYOND_HISTORY_RINBUFFER beyond_label, not_beyond_label
                ld a, h                                 ; Compare high byte of HL with high byte of history_ringbuffer_end
                cp history_ringbuffer_end >> 8
                jr C, not_beyond_label

                ld a, l                                 ; Compare low byte of HL with low byte of history_ringbuffer_end
                cp history_ringbuffer_end & 0xFF
                jr C, not_beyond_label
                jr beyond_label
        ENDM

        ; Tests if HL is a null byte
        ; Alters: A
        MACRO ON_HL_IS_NULL_GOTO label
                ld a, h
                cp l
                jp z, label
        ENDM


        ; Tests if DE is not equal to BC
        ; Alters: A
        MACRO DE_NE_BC label
        ld a, d
        cp b
        jr nz, label ; If high bytes are different, jump

        ld a, e
        cp c
        jr nz, label ; If low bytes are different, jump

        ; If we reach here, DE == BC (so we don't jump)
        ENDM


        ; Setup HL and BC for all zealline_history_search*
        ; Returns: HL and BC, the result values
        MACRO SETUP_SEARCH_RESULT _
                ld hl, history_search_result
                ld ix, (history_iterator_ptr)
                ld b, 0
                ld c, (ix+history_entry_line_len)
                dec c                                   ; dec c because remove nullbyte from length
        ENDM


        ; Resets the history serach iterator
        ; Alters: HL
zealline_reset_history_search:
        ld hl, 0
        ld (history_iterator_ptr), hl
        ret

        ; Searches backward through the history, retrieving the previous line.
        ; Returns: HL - the pointer to the line
        ;          BC - length of the line
        ; Alters: IX, A, HL
zealline_history_search_backward:
        push de
        ld bc, 0
        ld hl, (history_current_ptr)
        ON_HL_IS_NULL_GOTO(_history_search_backward_return)
        call history_iterator_back
        call copy_iterator_to_search_result
        SETUP_SEARCH_RESULT()
_history_search_backward_return:
        pop de
        ret


        ; Searches forward through the history, revriving the next line
        ; Returns: HL - the pointer to the line
        ;          BC - length of the line
        ; Alters: IX, A, HL
zealline_history_search_forward:
        push de
        ld bc, 0
        ld hl, (history_current_ptr)
        ON_HL_IS_NULL_GOTO(_history_search_forward_return)
        call history_iterator_forward
        call copy_iterator_to_search_result
        SETUP_SEARCH_RESULT()
_history_search_forward_return:
        pop de
        ret


        ; "zealline_add_history" stores a command to the history
        ;   Stores the NULL-terminated string from HL as into the ringbuffer.
        ;   In the case the ringbuffer is full old values will be removed from
        ;   in order to create space for the new line.
        ;
        ;   There is some kind of alignment for the history_entry struct that is
        ;   written to the ringbuffer which ensures that the "header" of the string
        ;   is never going across the end of the ringbuffer. This is achieved by
        ;   ensuring that the starting address of each entry is aligned to a 4-byte
        ;   boundary. Since the header is 3 bytes long (next pointer + length byte),
        ;   it will always fit within the remaining space before the boundary.
        ; Parameter:
        ;       HL - Pointer to the NULL-terminated string
        ; Alters: A, IX, IY
        ; Returns:
        ;   A  - ERR_SUCCESS on success, error value else
zealline_add_history:
        push hl
        push de
        push bc
        ld de, history_ringbuffer
        call strlen                                     ; BC is stringlength
        ld a, b
        or a
        jp nz, _add_history_error
        ld a, c                                         ; C is stringlength
        cp MAX_LINE_LENGTH
        jp nc, _add_history_error
        ex de, hl                                       ; store line in DE
        ld hl, (history_current_ptr)
        inc c                                           ; line length: Add 1 for NULL Byte
        ON_HL_IS_NULL_GOTO(_add_history_first_entry)    ; Add the first Element
        ; regular insert into the ringbuffer
        ld a, c
        ADD_HISTORY_ALIGNMENT()
        ld b, a                                         ; entry length: store in B
        ; Calculate the address were we are going to write to
        ld ix, (history_current_ptr)                    ; Load the address of the entry into HL and IX
        ld hl, ix
        ld a, (ix+history_entry_line_len)               ; Load the length of that entry into A
        ADD_HISTORY_ALIGNMENT()
        ADD_HL_A()                                      ; => HL points to the next address we want to write to
        ON_HL_BEYOND_HISTORY_RINBUFFER(_add_history_wrap_hl_into_ringbuffer, _add_history_check_for_space)
_add_history_wrap_hl_into_ringbuffer:
        ld ix, bc                                       ; STORE BC to IX
        ld bc, HISTORY_SIZE
        or a                                            ; Remove Carry Flag
        sbc hl, bc                                      ; subtract rinbuffer_size from HL
        ld bc, ix                                       ; RESTORE BC
_add_history_check_for_space:
        call is_history_space_available                 ; checks if we have enough space in the ringbuffer
        or a
        jp z, _add_history_add_entry
        ; Drop the element - Because of the alignment this is happening without potential wrap-around
        push de                                         ; save DE
        ld ix, (history_current_ptr)
        ld iy, (ix+history_entry_next)                  ; iy - address of entry that should be removed
        ld de, (iy+history_entry_next)                  ; de - address of the entry that becomes the new next entry
        ld (ix+history_entry_next), de                  ; store it to current entry
        pop de
        jp _add_history_check_for_space
_add_history_add_entry:
        ; Append history element to HL
        ld ix, hl                                       ; IX - address of new entry
        ld hl, (history_current_ptr)                    ; load current pointer - previous entry
        ld (history_current_ptr), ix                    ; write the new pointer.
        ; the entry at current_ptr should point to the element we are creating
        ; but it points to the next element, we need to point to that element!
        ASSERT history_entry_next==0
        ; inc hl, history_entry_next - is a nop because history_entry_next is 0
        ld iy, (hl)                                     ; address of next entry
        ; write the header of new entry
        ld a, iyl
        ld (ix+history_entry_next), a                   ; addr of next
        ld a, iyh
        ld (ix+history_entry_next+1), a                 ; addr of next
        ld (ix+history_entry_line_len), c
        ; write into previous entry the address of new entry
        ld iy, hl                                       ; load hl into iy for index adressing
        ld a, ixl
        ld (iy+history_entry_next), a
        ld a, ixh
        ld (iy+history_entry_next+1), a

        ; copy line (with wrap-around handling and null terminator check)
        ld hl, ix                               ; HL = destination address (IX)
        add hl, history_entry_line_ptr          ; HL = destination address + offset for string
_add_history_add_entry_copy_loop:
        ld a, (de)                              ; Load a byte from the string
        cp 0                                    ; Check for null terminator
        ld (hl), a                              ; Write the byte to the ring buffer
        jr z, _add_history_success              ; Return from the function if null terminator is encountered
        inc de                                  ; Increment DE (source address)
        inc hl                                  ; Increment HL (destination address)
        ON_HL_BEYOND_HISTORY_RINBUFFER(_add_history_set_hl_to_ringbuffer_start, _add_history_add_entry_copy_loop)
_add_history_set_hl_to_ringbuffer_start:
        ld hl, history_ringbuffer               ; Wrap around to the beginning of the buffer
        jr _add_history_add_entry_copy_loop
_add_history_first_entry:
        ld ix, history_ringbuffer
        ld hl, ix
        ld (ix+history_entry_next), l                   ; copy addres to self
        ld (ix+history_entry_next+1), h
        ld (ix+history_entry_line_len), c               ; line length with NULL byte
        ; copy line
        ld hl, ix
        add hl, history_entry_line_ptr                  ; hl - dest & de - string to copy
        ex de, hl                                       ; de - dest & hl - string to copy
        ld b, 0                                         ; bc - length of string with null-byte
        ldir
        ld hl, history_ringbuffer
        ld (history_current_ptr), hl                    ; point to the first entry
_add_history_success:
        call zealline_reset_history_search
        ld a, ERR_SUCCESS
        pop bc
        pop de
        pop hl
        ret
_add_history_error:
        ld a, ERR_FAILURE
        pop bc
        pop de
        pop hl
        ret


        ; ---------------------------------------------------------------------
        ; PRIVATE_FUNCTIONS (all to be call'ed)
        ; ---------------------------------------------------------------------

 ___just_for_tooltip:

        ; Turns the iterator one entry backwards
        ; Returns: BC - value of (history_iterator_pr)
        ; Alters: A, BC, DE, HL
history_iterator_back:
        ld hl, (history_iterator_ptr)
        ON_HL_IS_NULL_GOTO(_history_iterator_back_use_current_ptr)
        ld a, h
        or l
        call z, zealline_reset_history_search           ; also loads HL
        ld de, hl                                       ; store HL in DE, our destinatin if the prev element was found
        ASSERT history_entry_next==0                    ; so we can short-cut and use (hl) to load the next node
        ld bc, hl                                       ; just for the loop
_history_iterator_back_loop:
        ld hl, bc
        ld bc, (hl)                                     ; load next node into BR
        DE_NE_BC(_history_iterator_back_loop)           ; if next node is equal to DE then we have found it
        ld (history_iterator_ptr), hl
        ret
_history_iterator_back_use_current_ptr:
        ld hl, (history_current_ptr)
        ld (history_iterator_ptr), hl
        ret

        ; Turns the iterator one entry forward
        ; Alters: A, BC, HL
history_iterator_forward:
        ld hl, (history_iterator_ptr)
        ld a, h
        or l
        jp nz, _history_iterator_forward_get_next
        ld hl, (history_current_ptr)
_history_iterator_forward_get_next:
        ld bc, (hl)
        ld (history_iterator_ptr), bc
        ret

        ; copy the current iterator entry to history_search_result
        ; Alters: A, BC, DE, HL, IX
copy_iterator_to_search_result:
        ld ix, (history_iterator_ptr)
        ld b, 0
        ld c, (ix+history_entry_line_len)
        ld hl, ix
        add hl, history_entry_line_ptr
        ld de, history_search_result
_copy_iterator_to_search_result_copy_loop:
        ldi
        ld a, c
        or a
        ret z
        ON_HL_BEYOND_HISTORY_RINBUFFER(_copy_iterator_to_search_result_fix_hl, _copy_iterator_to_search_result_copy_loop)
_copy_iterator_to_search_result_fix_hl:
        ld hl, history_ringbuffer
        jr _copy_iterator_to_search_result_copy_loop


        ; is_history_space_available
        ; Parameters:
        ;       HL - Address want to write to
        ;       B - required space
        ; Returns:
        ;       A - 0 if we have enough space
        ;       A - 1 if we dont have enough space
is_history_space_available:
        push de
        push hl
        ASSERT history_entry_next==0
        ld ix, (history_current_ptr)            ; Address of current Node
        ld de, (ix)                             ; Address of the Next Node (where next of current is actually the oldest)
        or a            ; Remove carry flag
        ex hl, de       ; SWAP HL <-> DE
        sbc hl, de      ; Calculate the difference between DE (start address) and HL (oldest element)
        jp p, _history_space_available_positive ; If the result is negative, we need to wrap around
        add hl, HISTORY_SIZE                    ; therefore we add HISTORY_SIZE, now HL is thenumber of free bytes
_history_space_available_positive:
        ; Compare the available space (HL) with the required space (B)
        ld a, h
        or a
        jp nz, _history_space_available
        ld a, l            ; Compare the low byte of HL with B
        cp b
        jr c, _history_space_not_available  ; Not enough space
_history_space_available:
        xor a              ; Set A to 0
        pop hl
        pop de
        ret
_history_space_not_available:
        ld a, 1            ; Set A to a non-zero value (e.g., 1)
        pop hl
        pop de
        ret


        ; ---------------------------------------------------------------------
        SECTION BSS
        ; ---------------------------------------------------------------------

history_ringbuffer:         defs HISTORY_SIZE, 0
history_ringbuffer_end:
history_current_ptr:        defw 0 ; pointer into the history ringbuffer
history_iterator_ptr:       defw 0 ; pointer for the search iterator
history_search_result:      defs MAX_LINE_LENGTH, 0
