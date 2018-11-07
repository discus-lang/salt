
# Find all the source files in the project.
salt_src_hs  = $(shell find src/salt -name "*.hs" -follow)

# Build the executable.
bin/salt : $(salt_src_hs)
	@$(GHC) $(GHC_LANGUAGE) $(GHC_WARNINGS) $(GHC_FLAGS) \
		$(SALT_PACKAGES) -j$(THREADS) \
		-o bin/salt  \
		-isrc/salt --make src/salt/Main.hs