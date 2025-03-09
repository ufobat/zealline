; SPDX-FileCopyrightText: 2023 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0

        SECTION TEXT
        PUBLIC str_contains
        PUBLIC str_startswith
        PUBLIC strcmp
        PUBLIC strlen

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


; str_contains: Checks if a substring is present within a string.
; Parameters:
;       HL - Address of the main string (null-terminated).
;       DE - Address of the substring (null-terminated).
; Result:
;       A - 0 if the substring is found, 1 if it is NOT found.
; Alters: AF, BC, DE, HL, IX
str_contains:
        push hl          ; Save main string pointer
_str_contains_outer_loop:
        ld a, (hl)                      ; Load character from main string
        or a                            ; Check for null terminator
        jr z, _str_contains_not_found   ; If end of main string, not found
        call str_startswith             ; Compare substring with current portion of main string
        jr z, _str_contains_found       ; If strcmp returns 0 (strings are equal), substring found
        inc hl                          ; Move to next character in main string
        jr _str_contains_outer_loop
_str_contains_not_found:
        ld a, 1         ; Substring not found
        jp _str_contains_end
_str_contains_found:
        ld a, 0         ; Substring found
_str_contains_end:
        pop hl          ; Restore main string pointer
        ret

        ; Routine returning the length of a NULL-terminated string
        ; Parameters:
        ;   HL - NULL-terminated string to get the length from
        ; Returns:
        ;   BC - Length of the string
        ; Alters:
        ;   A, BC
strlen:
        push hl
        xor a
        ld b, a
        ld c, a
_strlen_loop:
        cp (hl)
        jr z, _strlen_end
        inc hl
        inc bc
        jr _strlen_loop
_strlen_end:
        pop hl
        ret