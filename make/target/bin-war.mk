
# Find all the source files in the project.
war_src_hs	= $(shell find src/war -name "*.hs" -follow)


# Build the executable.
bin/war : $(war_src_hs)
	@$(GHC) $(GHC_LANGUAGE) $(GHC_WARNINGS) $(GHC_FLAGS) \
		$(WAR_PACKAGES) \
		-threaded \
		-o bin/war  \
		-isrc/war --make src/war/Main.hs

