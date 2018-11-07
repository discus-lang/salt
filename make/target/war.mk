
.PHONY	: war
war	: bin/war bin/salt
	@echo "* Running tests"
	@bin/war test

.PHONY	: test
test	: bin/war bin/salt
	@echo "* Running tests"
	@bin/war test