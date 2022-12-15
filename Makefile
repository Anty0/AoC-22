# Compiler and flags
HC = ghc
HC_FLAGS = -threaded -dynamic -O3

# Directories
SRC_DIR = src
BUILD_DIR = build
OUT_DIR = out

# List of haskell scripts
SRCS = $(wildcard $(SRC_DIR)/*.hs)

# Generate list of output files
OUTS = $(patsubst $(SRC_DIR)/%.hs,$(OUT_DIR)/%,$(SRCS))

# Default target - build all scripts
all: $(OUTS)

# Remove all build files
clean:
	rm -r $(BUILD_DIR) $(OUT_DIR)

# all and clean are not file targets
.PHONY: all clean

# Rule for building each script
$(OUT_DIR)/%: $(SRC_DIR)/%.hs
	mkdir -p $(BUILD_DIR) $(OUT_DIR)
	$(HC) $(HC_FLAGS) -odir=$(BUILD_DIR)/$(basename $(notdir $<)) -hidir=$(BUILD_DIR)/$(basename $(notdir $<)) -o $@ $<
