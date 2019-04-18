# ----------------------------------------------------------------------
# Environment specific parameters
# ----------------------------------------------------------------------

-include local.mk

# Where to find the kerl executable
KERL ?= kerl

# List of OTP versions to build with (default: every kerl installations)
OTP_VSNS ?= $(shell $(KERL) list installations | cut -d' ' -f1)

# CPU frequency used for running the tests
CPU_FREQ ?= 2.6GHz

# ----------------------------------------------------------------------
# Helper variables
# ----------------------------------------------------------------------

SPACE :=
SPACE +=
COMMA := ,

# ----------------------------------------------------------------------
# Variables used when running performance tests
# ----------------------------------------------------------------------

# The number of CPU-s
CPU_CNT := $(shell nproc)

# The list of CPU ids
CPUS := $(shell seq 0 $$(( $(CPU_CNT) - 1 )))

# Flags to pass to the emulator
ERL_FLAGS := +K true +sbt db +scl false +sbwt very_long

ifdef OTP_VSN
# ----------------------------------------------------------------------
# Variables used when building with a specific OTP version
# ----------------------------------------------------------------------

# The OTP version-specific top-level build directory
BUILD_DIR := build/$(OTP_VSN)

# The OTP version-specific build directories
BUILD_DIRS := $(addprefix $(BUILD_DIR)/,asm mnesia ebin bapp bin)

# Erlang source files: compiled to both .S and .beam files
ERL_SRCS := $(wildcard src/*.erl)

# Bapp source files: compiled to .S and .beam files
BAPP_SRCS := $(wildcard asm/*.bapp)
BAPP_ASM_SRCS := $(addprefix $(BUILD_DIR)/asm/,$(notdir $(addsuffix .S,$(basename $(BAPP_SRCS)))))

# Test source files: compiled to .beam files only
TEST_SRCS := $(wildcard test/*.erl)

# Source files of the bapp escript
BAPP_XRL_SRCS := $(wildcard bapp/*.xrl)
BAPP_YRL_SRCS := $(wildcard bapp/*.yrl)
BAPP_ERL_GENS := $(addprefix $(BUILD_DIR)/bapp/,$(notdir $(addsuffix .erl,$(basename $(BAPP_XRL_SRCS) $(BAPP_YRL_SRCS)))))
BAPP_ERL_SRCS := $(wildcard bapp/*.erl) $(BAPP_ERL_GENS)
BAPP_BEAMS := $(addprefix $(BUILD_DIR)/bapp/,$(notdir $(addsuffix .beam,$(basename $(BAPP_ERL_SRCS)))))

# Source files of mnesia
MNESIA_SRCS := $(wildcard mnesia/*.erl)
MNESIA_ASMS := $(addprefix $(BUILD_DIR)/mnesia/,$(notdir $(addsuffix .S,$(basename $(MNESIA_SRCS)))))
STATS := $(BUILD_DIR)/opstats.csv

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
	@echo =========== $@ ===========
	. $(lastword $(shell $(KERL) list installations | grep '^$@ '))/activate && \
	$(MAKE) OTP_VSN=$@ build_with_otp test_with_otp

.PHONY: clean
clean:
	rm -rf build

.PHONY: test_with_otp
test_with_otp: build_with_otp
	$(MAKE) lockcpu
	erl $(ERL_FLAGS) -pa $(BUILD_DIR)/ebin -noshell -s perftest -s init stop
	$(MAKE) unlockcpu

.PHONY: build_with_otp
build_with_otp: $(BEAM_MARKER) $(ASM_MARKER) $(BUILD_DIR)/bin/bapp $(STATS)

$(BEAM_MARKER): $(ERL_SRCS) $(TEST_SRCS) $(BAPP_ASM_SRCS) | $(BUILD_DIR)/ebin
	$(if $(findstring test/perftest.erl,$?),erlc -o $(BUILD_DIR) test/perftest.erl,)
	erlc -pz $(BUILD_DIR) -o $(BUILD_DIR)/ebin $?
	touch $(BEAM_MARKER)

$(ASM_MARKER): $(ERL_SRCS) | $(BUILD_DIR)/asm
	erlc -o $(BUILD_DIR)/asm -S +no_line_info $?
	touch $(ASM_MARKER)

$(BUILD_DIR)/bin/bapp: $(BAPP_ERL_SRCS) | $(BUILD_DIR)/bin $(BUILD_DIR)/bapp
	erlc -o $(BUILD_DIR)/bapp $?
	cd $(BUILD_DIR)/bapp && \
	erl -noshell \
	    -noinput \
	    -eval 'ok = escript:create("../bin/bapp", [shebang, {archive, ["$(subst $(SPACE),"$(COMMA)",$(strip $(notdir $(BAPP_BEAMS))))"], []}]).' \
	    -eval 'init:stop().'
	chmod +x $@

$(BUILD_DIR)/bapp/%.erl: bapp/%.xrl | $(BUILD_DIR)/bapp
	erlc -o $(BUILD_DIR)/bapp $<

$(BUILD_DIR)/bapp/%.erl: bapp/%.yrl | $(BUILD_DIR)/bapp
	erlc -o $(BUILD_DIR)/bapp -v $<

$(BUILD_DIR)/asm/%.S: asm/%.bapp | $(BUILD_DIR)/bin/bapp $(BUILD_DIR)/asm
	$(BUILD_DIR)/bin/bapp -o $(BUILD_DIR)/asm $<

$(BUILD_DIRS):
	mkdir -p $@

$(STATS): $(MNESIA_ASMS) opstats
	./opstats $(MNESIA_ASMS) > $@

$(MNESIA_ASMS:.S=.%): $(MNESIA_SRCS) | $(BUILD_DIR)/mnesia
	erlc -o $(BUILD_DIR)/mnesia -S +no_line_info $?
	touch $(MNESIA_ASMS)

# ----------------------------------------------------------------------
# Helper rules for locking/unlocking CPU frequency
# ----------------------------------------------------------------------

.PHONY: lockcpu
lockcpu: $(addprefix lockcpu.,$(CPUS))
lockcpu.%:
	sudo cpufreq-set -c $* -g performance --min $(CPU_FREQ) --max $(CPU_FREQ)

.PHONY: unlockcpu
unlockcpu: $(addprefix unlockcpu.,$(CPUS))
unlockcpu.%:
	sudo cpufreq-set -c $* -g powersave --min $$(cpufreq-info -l | sed 's/ / --max /')
