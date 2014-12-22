## Process this file with automake to produce Makefile.in -*-Makefile-*-
AUTOMAKE_OPTIONS = foreign

dist_lisp_LISP = gnuplot.el gnuplot-gui.el gnuplot-context.el
EXTRA_DIST = dotemacs.el gpelcard.dvi gpelcard.pdf gpelcard.ps gpelcard.tex

CLEANFILES = gpelcard.pdf gpelcard.ps gpelcard.dvi gpelcard.log gpelcard.aux
DISTCLEANFILES =

.dvi.ps:
	@if [ "$(DVIPS)" != no ] ; then \
	  $(DVIPS) -o $@ $< ; \
	else \
	  echo "dvips not found: cannot make $@" ; \
	fi

.tex.dvi:
	@if [ "$(LATEX)" != no ] ; then \
	  $(LATEX) $< ; \
	else \
	  echo "latex not found: cannot make $@" ; \
	fi

.tex.pdf:
	@if [ "$(PDFLATEX)" != no ] ; then \
	  $(PDFLATEX) $< ; \
	else \
	  echo "pdflatex not found: cannot make $@" ; \
	fi

pdf: gpelcard.pdf
ps: gpelcard.ps

test: gnuplot.elc gnuplot-context.elc gnuplot-tests.elc gnuplot-test-context.elc
	emacs --batch -L .				\
	  --load=gnuplot-tests				\
	  --load=gnuplot-test-context			\
	  --eval='(ert-run-tests-batch "^gnuplot-")'

SUFFIXES = .el .elc .pdf .ps .tex
