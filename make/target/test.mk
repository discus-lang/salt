
.PHONY	: test
test	: bin/war bin/salt
	@echo "* Running expect tests"
	@bin/war test
	@echo "* Running property tests"
	@bin/waves
