#+TITLE: Gnuplot for Emacs

This package allows running [[http://www.gnuplot.info/][gnuplot]] files from within the [[https://www.gnu.org/software/emacs/][GNU Emacs]]
editor. It features:

- Syntax highlighting and indentation for gnuplot scripts.
- Pull-down menus for common gnuplot-related tasks.
- Interactive gnuplot sessions using =comint=.
- Context-sensitive completion.
- Inline display of gnuplot plots.

It is recommended to run =gnuplot-mode= on GNU Emacs 25 or above,
using gnuplot version 5.0 or above.

* Installation
** Using MELPA

The easiest way to install =gnuplot-mode= is to directly get it from
[[http://melpa.milkbox.net][MELPA]]. After [[http://melpa.milkbox.net/#installing][configuring Emacs to use MELPA]], you should be able to
install gnuplot-mode by typing

: M-x install-package RET gnuplot RET

or do =M-x list-packages= and search for =gnuplot= in the list.

** Using =el-get=

The [[https://github.com/dimitri/el-get.git][el-get]] package includes a gnuplot-mode recipe. So to install
simply call

: M-x el-get-install RET gnuplot-mode

Alternatively, you can directly place the following in your init file so that
=el-get= can install and load gnuplot-mode at Emacs start up:

: (el-get 'sync 'gnuplot-mode)

** From source

After fetching the package's source from [[https://github.com/emacsorphanage/gnuplot][the homepage]], byte-compile
the package's files using the =make= command and move the compiled
=.elc= files to your chosen target directory.

* Configuration
** Load Path

First of all, make sure that =gnuplot.el= is in your load-path (this is automatic if using a package helper like use-package). To do so manually, add the following snippet in your emacs configuration file

#+begin_src emacs-lisp
(add-to-list 'load-path "/path/to/gnuplot")
#+end_src

** Info File

The function =gnuplot-info-lookup-symbol= looks at the Gnuplot info
file. For that function to work, a =gnuplot.info= file must be placed
somewhere where info can find it. You can either use the file provided
by this package or a locally installed one. The following snippet
allows you to put the =gnuplot.info= any place convenient:

#+begin_src emacs-lisp
(add-to-list 'Info-default-directory-list "/path/to/info/file")
#+end_src

** Enable Mode

You can automatically enable =gnuplot-mode= using the snippet below:

#+begin_src emacs-lisp
(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
#+end_src

* Usage

Apart from enabling =gnuplot-mode= automatically (see above), these
two functions are useful for starting up gnuplot-mode:

- =M-x gnuplot-mode= : start gnuplot-mode in the current buffer
- =M-x gnuplot-make-buffer= : open a new buffer (which is not visiting
  a file) and start gnuplot-mode in that buffer


** Bindings

When =gnuplot-mode= is on, the following keybindings are available:

| C-c C-l        | send current line to gnuplot                         |
| C-c C-v        | send current line to gnuplot and move forward 1 line |
| C-c C-r        | send current region to gnuplot                       |
| C-c C-b        | send entire buffer to gnuplot                        |
| C-c C-f        | send a file to gnuplot                               |
| C-c C-i        | insert filename at point                             |
| C-c C-n        | negate set option on current line                    |
| C-c C-c        | comment region                                       |
| C-c C-o        | set arguments for command at point                   |
| S-mouse-2      | set arguments for command under mouse cursor         |
| C-c C-d        | read the gnuplot info file                           |
| C-c C-e        | show-gnuplot-buffer                                  |
| C-c C-k        | kill gnuplot process                                 |
| C-c C-z        | customize gnuplot-mode                               |
| M-tab or M-ret | complete keyword before point                        |
| ret            | newline and indent                                   |
| tab            | indent current line                                  |


With the exception of the commands for sending commands to Gnuplot,
most of the above commands also work in the Gnuplot comint buffer, in
addition to the following:

| M-C-p   | plot the most recent script buffer line-by-line   |
| M-C-f   | save the current script buffer and load that file |
| C-c C-e | pop back to most recent script buffer             |

** Context-sensitive keyword completion

By default gnuplot-mode will try to parse your commands as you type
and suggest only relevant completion candidates on typing =M-TAB= or
=TAB=. For example, with point after the =with= of a =plot= command,
tab completion will suggest only plotting styles. This also enables
more specific help topic lookup in the gnuplot info file, provided you
have the right version of =gnuplot.info= installed (see the
Installation section for isntructions).

If the context-sensitivity annoys you, you can get simple
non-context-sensitive completion back by toggling
=gnuplot-context-sensitive-mode=. See also the variable
=gnuplot-tab-completion=.

By its nature, the completion code has to know a fair bit about the
structure of the gnuplot language. If you use it with an old version
of gnuplot (pre version 4) it will make mistakes. Most of gnuplot
4.6's command language is parsed correctly except for the =set
terminal= commands.

** Eldoc mode

If you install the file =gnuplot-eldoc.el= from a recent Gnuplot
distribution, gnuplot-mode can show syntax hints in the modeline when
~eldoc-mode~ is turned on and context sensitivity is enabled.

** Inline Images

You can optionally have plots displayed inline in the Gnuplot comint
process buffer. This is handy for trying things out without having to
switch between Emacs and the Gnuplot display. Call
=gnuplot-inline-display-mode= in a gnuplot-mode buffer to try it out. This
feature is implemented using temporary =png= files, and is also somewhat
experimental. It requires Gnuplot to have =png= support and a GNU Emacs
with image support. Please report bugs.



* FAQ / Remarks

** Usage on Windows

Multiple users have reported issues when trying to work with
=gnuplot.el= on Windows. Most notably, the gnuplot process hangs after
sending a first line of input (this is a common Emacs issue on
Windows, see [[https://www.gnu.org/software/emacs/manual/html_mono/efaq-w32.html#Sub_002dprocesses][here]]).

A partial workaround was to use =pgnuplot.exe= as the
=gnuplot-program=. However, =pgnuplot.exe= is not included with
gnuplot since version 5.0.

You currently have two solutions:

1. Experiment using the =gnuplot-program= and =gnuplot-program-args=
   variables. For instance, setting

   #+begin_src emacs-lisp
(setq gnuplot-program "/path/to/cmdproxy.exe")
(setq gnuplot-program-args "/C /path/to/gnuplot.exe")
   #+end_src

   has been reported to work (see [[https://github.com/emacsorphanage/gnuplot/pull/33/files][here]] for a reference).

2. Use the simpler [[https://github.com/mkmcc/gnuplot-mode][gnuplot-mode]] package that sends the entire buffer
   to gnuplot. Since no =comint= is involved, it should function
   correctly, but you lose most features of the =gnuplot.el= package.
   We would like to implement a send-buffer without comint as well
   eventually.

More information on =gnuplot.el= and Windows can be found on these threads: [[https://github.com/emacsorphanage/gnuplot/issues/15][1]], [[https://github.com/emacsorphanage/gnuplot/pull/33][2]]


** Pause Command

Gnuplot's =pause -1= command, which waits for the user to press a key,
is problematic when running under Emacs. Sending =pause -1= to the
running gnuplot process will make Emacs appear to freeze. (It isn't
really crashed: typing =C-g= will unlock it and let you continue). The
workaround for now is to make Gnuplot output a string before pausing,
by doing =pause -1 "Hit return"= or similar.

** Issue with Unicode Character Display

Some users have reported [[https://github.com/emacs-gnuplot/gnuplot/issues/39][issues when trying to display unicode characters]]. This issue is likely due to your distribution bundling gnuplot with [[https://unix.stackexchange.com/questions/496206/unicode-in-gnuplot-terminal/496245#496245][editline instead of readline]]. Recompiling the source with support for unicode should fix the issue until this issue is fixed upstream. Thanks to [[https://github.com/rolandog][rolandog]] for discovering this fix.
