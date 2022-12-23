
# Compiler to use
GHC = ghc-9.2.5

# Flags to pass to the compiler
GHC_FLAGS = -O2

# Rule for building object files from source files
all:
	$(GHC) $(GHC_FLAGS) RockPaperScissors.hs

# Clean target
.PHONY: clean
clean:
	rm -f $(TARGET) $(OBJECTS)
