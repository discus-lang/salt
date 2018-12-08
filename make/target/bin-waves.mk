
# Find all the source files in the project.
waves_src_hs	= $(shell find src/waves -name "*.hs" -follow)


# Build the executable.
# Note: for some reason this target is producing dyn_* files, but the bin/salt and bin/war targets do not. I suspect that the dependency on hedgehog is somehow forcing dynamic linking.
bin/waves : $(salt_src_hs) $(waves_src_hs)
	@$(GHC) $(GHC_LANGUAGE) $(GHC_WARNINGS) $(GHC_FLAGS) \
		$(WAVES_PACKAGES) -j$(THREAD) \
		-threaded \
		-o bin/waves  \
		-isrc/salt \
		-isrc/waves --make src/waves/Main.hs

