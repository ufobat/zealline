#!/usr/bin/env python3
import pathlib
import re

current_dir = pathlib.Path(__file__).parent.resolve()
source_dir = current_dir / "src"
include_file = current_dir / "lib" / "zealline.asm"

def get_documentation(function_name, lines):
    regex = r"^\s*" + function_name + ":"
    take_me = 0
    documentation = []
    for line in reversed(lines):
        if re.search(regex, line):
            take_me = 1
        elif take_me == 1:
            if re.search(r"^\s*;", line):
                documentation.insert(0, line)
            else:
                return documentation
    return documentation


def parse_sourcefile(sourcefile):
   with sourcefile.open() as fh:
        lines = fh.readlines()
        result = []
        for line in lines:
                if m := re.search(r"^\s*PUBLIC\s+(\w+)", line):
                    function_name = m.group(1)
                    result.extend(get_documentation(function_name, lines))
                    result.append(f"\tEXTERN {function_name}\n\n")
        return result


with include_file.open(mode="w") as fh:
    for sourcefile in source_dir.glob("zeal*.asm"):
        fh.write(";;;; functions of " + sourcefile.name +
                 "\n\n" + "".join(parse_sourcefile(sourcefile)))