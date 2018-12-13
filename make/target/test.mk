
.PHONY	: test
test	: bin/war bin/salt bin/waves
	@echo "* Running expect tests"
	@bin/war test
	@echo "* Running property tests"
	@bin/waves
