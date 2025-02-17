	include "zos_sys.asm"

PUBLIC OutputNewline
PUBLIC OutputRegisters
PUBLIC OutputMemoryAtDE

	; Outputs a single new line
    ; Parameters: (none)
    ; Returns: (nothing)
    ; Alters: (nothing)
OutputNewline:
	push af
	push bc
	push de
	push hl
	ld h, 0
	ld de, Newline
	ld bc, 1
	WRITE()
	pop hl
	pop de
	pop bc
	pop af
	ret

	; Outputs all Registers
    ; Parameters: (none)
    ; Returns: (nothing)
    ; Alters: (nothing)
OutputRegisters:
	ld (StackPointer), sp
	push af
	push bc
	push hl

	; Register A
	; A is still untouched
	ld b, 2
	ld hl, ABuffer
	call convert_byte
	
	; Register B
	ld hl, sp
	add hl, 2
	ld hl, (hl)
	ld a, h
	ld b, 2
	ld hl, BBuffer	
	call convert_byte;

	; Register C
	ld hl, sp
	add hl, 2
	ld hl, (hl)
	ld a, l
	ld b, 2
	ld hl, CBuffer
	call convert_byte

	; Register D - is still untouched
	ld a, d
	ld b, 2
	ld hl, DBuffer	
	call convert_byte;

	; Register E
	ld a, e
	ld b, 2
	ld hl, EBuffer
	call convert_byte

	; Register H
	ld hl, sp
	ld hl, (hl)
	ld a, h
	ld b, 2
	ld hl, HBuffer	
	call convert_byte;

	; Register L
	ld hl, sp
	ld hl, (hl)
	ld a, l
	ld b, 2
	ld hl, LBuffer
	call convert_byte

	; StackPointer
	ld a, (StackPointer)
	ld b, 2
	ld hl, SPlBuffer
	call convert_byte
	ld a, (StackPointer+1)
	ld b, 2
	ld hl, SPhBuffer
	call convert_byte

	push de
	call print_register_message
	pop de
	pop hl
	pop bc
	pop af
	ret

    ; converts byte and stores it into buffer
    ; Parameters:
    ;       A - byte to convert
	;       B - length of bytes
	;       HL - ptr to buffer
    ; Returns: (nothing)
    ; Alters:
    ;       A, HL, DE, BC
convert_byte:
	; converts A and stores it into HL
	; rotate top nibble down
	rrc a
	rrc a
	rrc a
	rrc a
	push af             ; protect a from being modified by convert_nibble
	; convert the nibble nibble
    and 0x0f
    add a, '0'  ; ASCII-Offset für Ziffern
    cp a, '9'+1  ; Überprüfen, ob Ziffer > 9
    jr c, _convert_nibble_end ; Wenn < 9, dann fertig
    add a, 7   ; ASCII-Offset für Buchstaben A-F
_convert_nibble_end:
	ld (hl), a          ; store to buffer
	inc hl
	pop af
	djnz convert_byte
	ret

    ; prints out the RegisterMessage Lable
    ; Parameters:
    ;       L - Cursor X position
    ; Returns:
    ;       A - 0 (ERR_SUCCESS)
    ; Alters:
    ;       A, H, DE, BC
print_register_message:
	ld h, 0 ; u8 dev
	ld de, RegisterMessage ; u16 Message
	ld bc, RegistersEnd - RegisterMessage
	WRITE()
	ret

    ; Output a Memory dump (16 bytes) ad address in DE
    ; Parameters:
    ;       DE - Memory Address
    ; Returns: (nothing)
    ; Alters:  (nothing)
OutputMemoryAtDE:
	push af
	push bc
	push hl
	; SHOW address
	ld a, d
	ld b, 2
	ld hl, AddressBuffer
	call convert_byte
	ld a, e
	ld b, 2
	ld hl, AddressBuffer+2
	call convert_byte
	push de
	ld h, 0
	ld de, AddressMessage
	ld bc, AddressMessageEnd - AddressMessage
	WRITE()
	pop de
	ld b, 0x10        ; output 16 bytes
_output_memory_loop:
	push bc           ; save Bc for _output_memory_loop
	ld b, 2
	ld hl, ByteBuffer ; set destination for converted string
	ld a, (de)        ; load memory into a
	call convert_byte ; uses A, B and HL
	inc de
	push de           ; store DE for input
	ld h, 0           ; output ByteBuffer
	ld de, ByteBuffer
	ld bc, 3
	WRITE()           ; overwrites a
	pop de            ; restore DE for loop
	pop bc            ; restore Bc for loop
	djnz _output_memory_loop
	call OutputNewline
	pop hl
	pop bc
	pop af
	ret


;;; buffers
StackPointer: db 2, 0
RegisterMessage: defm "A: 0x"
ABuffer:  defs 2, 0
defm " BC: 0x"
BBuffer: defs 2, 0
CBuffer: defs 2, 0 
defm " DE: 0x"
DBuffer: defs 2, 0
EBuffer: defs 2, 0
defm " HL: 0x"
HBuffer: defs 2, 0
LBuffer: defs 2, 0
defm " SP: 0x"
SPhBuffer: defs 2, 0
SPlBuffer: defs 2, 0
Newline: defm "\n"
RegistersEnd:
ByteBuffer: defs 2, 0
WhiteSpace: defm " "
AddressMessage: defm "0x"
AddressBuffer: defs 4, 0
defm ": "
AddressMessageEnd: