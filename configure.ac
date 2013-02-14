#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.61])
AC_INIT([gnuplot-mode], [0.7], [bruceravel1@gmail.com])
AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_CHECK_PROGS(DVIPS, dvips, no)
AC_CHECK_PROGS(LATEX, latex latex2e, no)
AC_CHECK_PROGS(PDFLATEX, pdflatex, no)

# Check for Emacs and the Emacs site-lisp directory
AM_PATH_LISPDIR

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
