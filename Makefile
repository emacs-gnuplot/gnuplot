LOAD = -l gnuplot \
	-l gnuplot-context \
	-l gnuplot-debug-context \
	-l gnuplot-gui \
	-l gnuplot-test

.PHONY: all default clean regen test

default: compile

test:
	emacs --batch -L test -L . -f package-initialize $(LOAD) -f ert-run-tests-batch-and-exit

compile:
	emacs --batch -L test -L . -f package-initialize -f batch-byte-compile gnuplot-*.el test/gnuplot-*.el

clean:
	rm -f *.elc */*.elc
