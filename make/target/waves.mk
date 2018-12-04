
.PHONY	: waves
waves	: bin/waves
	@echo "* Running property tests"
	@bin/waves

