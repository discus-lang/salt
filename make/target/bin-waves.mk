
# Find all the source files in the project.
waves_src_hs	= $(shell find src/waves -name "*.hs" -follow)


# Build the executable.
bin/waves : $(salt_src_hs) $(waves_src_hs)
	$(GHC) $(GHC_LANGUAGE) $(GHC_WARNINGS) $(GHC_FLAGS) \
		$(WAVES_PACKAGES) \
		-threaded \
		-o bin/waves  \
		-isrc/salt \
		-isrc/waves --make src/waves/Main.hs

