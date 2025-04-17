; SPDX-FileCopyrightText: 2025 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)
;
; SPDX-License-Identifier: Apache-2.0


        SECTION TEXT
        PUBLIC zealline_to_uppercase

        ; "zealline_to_uppercase" reads a line/command from STDIN
        ;   Converts a Zeal Scancode in Register B to its uppercase
        ; Parameters:
        ;   B - the lowercase Zeal OS scancode
        ; Returns:
        ;   A  - In uppercase or Null if no uppercase is avaible
        ; Alters:
        ;   A
zealline_to_uppercase:
        push hl
        push bc
        ld c, b
        ld b, 0 ; set the high byte to 0
        ld hl, upper_case
        add hl, bc ; plus C
        ld a, (hl)
        pop bc
        pop hl
        ret


;;; AUTOGENERATED BY zealscancode.py ;;;
;;; US Keyboard Layout
       SECTION DATA
upper_case:
        defb  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 
        defb  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 
        defb ' ',  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 
        defb '(', '!', '@', '#', '$', '%', '^', '&', '*',  0 , ';',  0 ,  0 , '+',  0 ,  0 
        defb  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 
        defb  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 
        defb '"', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O'
        defb 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',  0 ,  0 ,  0 ,  0 ,  0 
