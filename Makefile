# SPDX-FileCopyrightText: 2025 Zeal 8-bit Computer <contact@zeal8bit.com>, Martin Barth (github:ufobat)#
#
# SPDX-License-Identifier: Apache-2.0
#

SHELL := /bin/bash

# binaries
ASM  := z88dk-z80asm

# directories
SRC_DIR          := src
BUILD_DIR        := build
LIB_DIR          := lib

# libraries to build
ASM_LIB_FILE     := zealline.lib
ASM_INCLUDE_FILE := zealline.asm

SOURCES          = $(shell find $(SRC_DIR) -name "*.asm")
OBJECTS          = $(patsubst $(SRC_DIR)/%.asm,$(BUILD_DIR)/%.o,$(SOURCES))

ifndef ZOS_PATH
$(error "Please define ZOS_PATH environment variable. It must point to Zeal 8-bit OS source code path.")
endif

# Specify Z80 as the target, compile without linking, and place all the code in TEXT section
# (_CODE must be replace).
ASMFLAGS  := -mz80 -I$(ZOS_PATH)/kernel_headers/z88dk-z80asm 

.PHONY: all clean cleanall

all: $(BUILD_DIR) $(SDCC_BUILD_DIR) $(LIB_DIR)/$(ASM_LIB_FILE) $(LIB_DIR)/$(ASM_INCLUDE_FILE) $(LIB_DIR)/$(SDCC_LIB_FILE)
	@bash -c 'echo -e "\x1b[32;1mSuccess, libraries generated\x1b[0m"'

# create ASM object files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.asm  | $(BUILD_DIR)/$(dir $<)
	@mkdir -p $(dir $@)
	$(ASM) $(ASMFLAGS) $< -o$@

	
$(BUILD_DIR):
	@echo "Creating build dir: $@"
	@mkdir -p $(BUILD_DIR)

$(LIB_DIR):
	@echo "Creating lib dir: $@"
	@mkdir -p $(LIB_DIR)

$(LIB_DIR)/$(ASM_LIB_FILE): $(OBJECTS) | $(LIB_DIR)
	@echo "Archiving library: $@"
	$(ASM) $(ASMFLAGS) -x$@ $(OBJECTS)

$(LIB_DIR)/$(ASM_INCLUDE_FILE): $(LIB_DIR)
	python3 zealinclude.py

clean:
	@echo "Cleaning build directory..."
	@rm -rf $(BUILD_DIR)

cleanall: clean
	@echo "Cleaning lib directory..."
	@rm -rf $(LIB_DIR)
