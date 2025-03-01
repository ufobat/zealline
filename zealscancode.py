#!/usr/bin/env python3
"""
    Generates code, a dispatch table to convert ZEAL OS Scancodes into Upper Case caracters
"""

def main():
    print(";;; AUTOGENERATED BY zealscancode.py ;;;")
    print(";;; US Keyboard Layout")
    print("       SECTION DATA")
    print("upper_case:")
    for line_number in range(8):
        defline(line_number)


def defline(line_number):
    print("        defb", end="")
    for byte_number in range (16):
        defbyte(line_number * 16 + byte_number)
    print()


dispatch = {
    ':': ';',
    '`': '"',
    '=': '+',
    ' ': ' ', 
}
def defbyte(offset):
    end = "," if (offset + 1) % 16 else ""
    char = chr(offset)
    if 48 <= offset <= 56:
        # numbers 0..9
        shifted_chars_string = "(!@#$%^&*"
        print(f" '{  shifted_chars_string[offset - 48]  }'", end=end)
    elif 97 <= offset <= 122:
        # letters
        print(f" '{chr(offset).upper()}'", end=end)
    # all others dont have a upper case: Null Byte
    elif char in dispatch:
        print(f" '{dispatch[char]}'", end=end)
    else:
        print("  0 ", end=end)

if __name__ == '__main__':
    main()