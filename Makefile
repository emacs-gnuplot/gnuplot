EMACS ?= emacs

LOAD = -l gnuplot \
	-l gnuplot-context \
	-l gnuplot-debug-context \
	-l gnuplot-gui \
	-l gnuplot-tests

.PHONY: all default clean

default: compile

prepare:
	$(EMACS) --eval "(progn (package-refresh-contents) (package-install 'compat))"

test: prepare
	$(EMACS) --batch -L . -f package-initialize $(LOAD) -f ert-run-tests-batch-and-exit

compile: prepare
	$(EMACS) --batch -L . -f package-initialize -f batch-byte-compile gnuplot-*.el

clean:
	rm -f *.elc
