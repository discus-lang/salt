
.PHONY	: war
war	: bin/war bin/salt
	@echo "* Running expect tests"
	@bin/war test
