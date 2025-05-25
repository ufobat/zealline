# SPDX-FileCopyrightText: 2025 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)#
#
# SPDX-License-Identifier: Apache-2.0
#

SHELL := /bin/bash
ASM_LIB_DIR = ../../lib
ASM_LIB_FILE = $(ASM_LIB_DIR)/zealline.lib
STRUTILS_LIB_FILE = $(ZOS_PATH)/kernel_headers/z88dk-z80asm/lib/strutils.lib

ifndef ZOS_PATH
$(error "Please define ZOS_PATH environment variable. It must point to Zeal 8-bit OS source code path.")
endif

ASM = z88dk-z80asm
ASMFLAGS := -mz80 -I$(ZOS_PATH)/kernel_headers/z88dk-z80asm -I$(ZOS_PATH)/kernel_headers/z88dk-z80asm/include -I$(ASM_LIB_DIR)

.PHONY: all clean cleanal

all: shell.bin

shell.bin:
	$(ASM) $(ASMFLAGS) -l$(ASM_LIB_FILE) -l$(STRUTILS_LIB_FILE) -b shell.asm -oshell.bin

clean:
	rm -rf shell.bin shell.o
