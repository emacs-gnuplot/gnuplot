EMACS ?= emacs

LOAD = -l gnuplot \
	-l gnuplot-context \
	-l gnuplot-debug-context \
	-l gnuplot-gui \
	-l gnuplot-tests

.PHONY: all default clean

default: compile

test:
	$(EMACS) -batch -L . $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -batch -L . -f batch-byte-compile gnuplot-*.el

clean:
	rm -f *.elc
