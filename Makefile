# ----------------------------------------------------------------------
# Environment specific parameters
# ----------------------------------------------------------------------

# Where to find the kerl executable
KERL ?= kerl

# List of OTP versions to build with (default: every kerl installations)
OTP_VSNS ?= $(shell $(KERL) list installations | cut -d' ' -f1)

ifdef OTP_VSN
# ----------------------------------------------------------------------
# Variables used when building with a specific OTP version
# ----------------------------------------------------------------------

# The OTP version-specific top-level build directory
BUILD_DIR := build/$(OTP_VSN)

# The OTP version-specific build directories
BUILD_DIRS := $(addprefix $(BUILD_DIR)/,asm ebin)

# Erlang source files: compiled to both .S and .beam files
ERL_SRCS := $(wildcard src/*.erl)

# Assembler source files: compiled to .beam files
ASM_SRCS := $(wildcard asm/*.S)

# Test source files: compiled to .beam files only
TEST_SRCS := $(wildcard test/*.erl)

# Marker files used for compiling multiple sources in one go
BEAM_MARKER := $(BUILD_DIR)/.beam-marker
ASM_MARKER := $(BUILD_DIR)/.asm-marker

endif

# ----------------------------------------------------------------------
# Build rules
# ----------------------------------------------------------------------

.PHONY: all
all: $(OTP_VSNS)

.PHONY: $(OTP_VSNS)
$(OTP_VSNS):
	. $(lastword $(shell $(KERL) list installations | grep '^$@ '))/activate && \
	$(MAKE) OTP_VSN=$@ build_with_otp

.PHONY: clean
clean:
	rm -rf build

.PHONY: build_with_otp
build_with_otp: $(BEAM_MARKER) $(ASM_MARKER)

$(BEAM_MARKER): $(ERL_SRCS) $(TEST_SRCS) $(ASM_SRCS) | $(BUILD_DIR)/ebin
	$(if $(findstring test/perftest.erl,$?),erlc -o $(BUILD_DIR) test/perftest.erl,)
	erlc -pz $(BUILD_DIR) -o $(BUILD_DIR)/ebin $?
	touch $(BEAM_MARKER)

$(ASM_MARKER): $(ERL_SRCS) | $(BUILD_DIR)/asm
	erlc -o $(BUILD_DIR)/asm -S $?
	touch $(ASM_MARKER)

$(BUILD_DIRS):
	mkdir -p $@
