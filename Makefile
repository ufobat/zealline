# SPDX-FileCopyrightText: 2025 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)#
#
# SPDX-License-Identifier: Apache-2.0
#

SHELL := /bin/bash

# binaries
CC   := sdcc
AR   := sdar
ASM  := z88dk-z80asm
SDAS := sdasz80

# directories
SRC_DIR          := src
SDCC_SRC_DIR     := sdcc/src
BUILD_DIR        := build
SDCC_BUILD_DIR   := sdcc/build
LIB_DIR          := lib

# libraries to build
ASM_LIB_FILE     := zealline.lib
SDCC_LIB_FILE    := zealline_sdcc_bindings.lib

SOURCES          = $(shell find $(SRC_DIR) -name "*.asm")
OBJECTS          = $(patsubst $(SRC_DIR)/%.asm,$(BUILD_DIR)/%.o,$(SOURCES))

SDCC_SOURCES     = $(shell find $(SDCC_SRC_DIR) -name "*.asm" )
SDCC_RELOCATIONS = $(patsubst $(SDCC_SRC_DIR)/%.asm,$(SDCC_BUILD_DIR)/%.rel,$(SDCC_SOURCES))

ifndef ZOS_PATH
$(error "Please define ZOS_PATH environment variable. It must point to Zeal 8-bit OS source code path.")
endif

# Specify Z80 as the target, compile without linking, and place all the code in TEXT section
# (_CODE must be replace).
CFLAGS    := -mz80 -c --codeseg TEXT -I$(ZOS_PATH)/kernel_headers/sdcc/include/ --opt-code-speed
ASMFLAGS  := -mz80 -I$(ZOS_PATH)/kernel_headers/z88dk-z80asm 
SDASFLAGS := -f -c -o

.PHONY: all clean cleanall

all: $(BUILD_DIR) $(SDCC_BUILD_DIR) $(LIB_DIR)/$(ASM_LIB_FILE) $(LIB_DIR)/$(SDCC_LIB_FILE)
	@bash -c 'echo -e "\x1b[32;1mSuccess, libraries generated\x1b[0m"'

# create ASM object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.asm  | $(BUILD_DIR)/$(dir $<)
	@mkdir -p $(dir $@)
	$(ASM) $(ASMFLAGS) $< -o$@

	
$(BUILD_DIR):
	@echo "Creating build dir: $@"
	@mkdir -p $(BUILD_DIR)

$(SDCC_BUILD_DIR):
	@echo "Creating sdcc build dir: $@"
	@mkdir -p $(SDCC_BUILD_DIR)

$(LIB_DIR):
	@echo "Creating lib dir: $@"
	@mkdir -p $(LIB_DIR)

$(LIB_DIR)/$(ASM_LIB_FILE): $(OBJECTS) | $(LIB_DIR)
	@echo "Archiving library: $@"
	$(ASM) $(ASMFLAGS) -x$@ $(OBJECTS)

$(LIB_DIR)/$(SDCC_LIB_FILE): $(SDCC_RELOCATIONS) | $(LIB_DIR)
	@echo "Archiving library: $@"
	$(AR) -rc $@  $(SDCC_RELOCATIONS)

$(SDCC_BUILD_DIR)/%.rel: $(SDCC_SOURCES)
	$(SDAS) $(SDASFLAGS) $@ $^

clean:
	@echo "Cleaning build directory..."
	@rm -rf $(BUILD_DIR)
	@rm -rf $(SDCC_BUILD_DIR)

cleanall: clean
	@echo "Cleaning lib directory..."
	@rm -rf $(LIB_DIR)
