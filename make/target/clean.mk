
# Cleanup everything.
.PHONY	: clean
clean	:
	@echo "* Cleaning up"
	@find . \( -name "*.o" -o -name "*.hi" -o -name "war-std" \) -follow -not -path "./.stack-work/*" \
		| xargs -n 1 rm -rf
	@rm -f make/deps/*
	@rm -f bin/*


# Cleanup just the test directory.
.PHONY	: clean-test
clean-test :
	@echo "* Cleaning up test"
	@find test -name "*.o" -o -name "*.hi" -o -name "war-std" -follow \
		| xargs -n 1 rm -rf

