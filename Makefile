
# Build the system, but don't automatically run tests.
.PHONY	: all
all	:
	@make allWithConfig


# Include configuration.
# These need to come before all the rules after this point in the Makefile.
include make/config.mk


# Build everything in scope of the make config.
.PHONY	: allWithConfig
allWithConfig :
	@make bin/salt
	@make bin/war
	@make bin/waves


include make/target/clean.mk
include make/target/setup.mk
include make/target/bin-salt.mk
include make/target/bin-war.mk
include make/target/bin-waves.mk
include make/target/war.mk
include make/target/test.mk
include make/target/waves.mk


# Override default config with local config.
-include make/config-override.mk
