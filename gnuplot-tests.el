;;; Tests for gnuplot-mode. -*- lexical-binding: t -*-

;;; Currently these attempt to cover the correct identification of
;;; string and comment syntax.

(require 'ert)
(require 'cl-lib)

;; Hide an annoying interactive message during batch testing
(when (require 'nadvice nil t)
  (advice-add
   'message
   :around
   (lambda (orig-message format-string &rest args)
     (unless (string= format-string
                      "gnuplot-mode %s (gnuplot %s) -- report bugs with %S")
       (apply orig-message format-string args)))))

(eval-and-compile
  (defvar gnuplot-string-test-contexts
    '("%s"
      "print %s"
      "print %s, 2+3"
      "print %s, 'another string'"
      "print %s, \"another string\""
      "print 'another string', %s"
      "\"double-quoted string\" %s"
      "\"double-quoted string\"%s"
      "%s \"double-quoted string\""
      "%s\"double-quoted string\""
      "'single-quoted string' %s"
      "%s 'single-quoted string'"
      "'single-quoted string' %s 'single-quoted string'")))

(defun gnuplot-test-string-in-context (string context)
  "Test syntax-propertizing of STRING in CONTEXT in gnuplot-mode.

STRING contains text representing a Gnuplot string literal.
CONTEXT is a context is a context to place the literal within,
represented by a format-string with a single %s placeholder.

Returns non-nil if STRING is correctly recognised as a single
string by `scan-sexps'."
  (cl-destructuring-bind (prologue epilogue)
      (split-string context "%s")
    (with-temp-buffer
      (gnuplot-mode)
      (let (start end)
        (save-excursion
          (insert prologue)
          (setq start (point))
          (insert string)
          (setq end (point))
          (insert epilogue))
        (syntax-propertize (point-max))
        (string= (buffer-substring start (scan-sexps start 1))
                 string)))))

(defmacro gnuplot-test-string (name string)
  "Define an `ert' test to check syntax recognition of STRING in gnuplot-mode.

The test checks that STRING is correctly recognised as a single
string-literal in multiple different contexts, as determined by
`gnuplot-string-test-contexts'."
  (declare (indent 1))
  `(ert-deftest ,name ()
     ,string
     ,@(cl-loop for context in gnuplot-string-test-contexts
             collect
             `(should (gnuplot-test-string-in-context ,string ,context)))))


;;;; Tests for double-quoted strings
(gnuplot-test-string gnuplot-double-quoted-string
                     "\"double-quoted string\"")

(gnuplot-test-string gnuplot-double-quoted-with-single-quotes
                     "\"double-quoted 'with single quotes' embedded\"")

(gnuplot-test-string gnuplot-double-quoted-with-single-quotes-2
                     "\"'single quotes inside double quotes'\"")

(gnuplot-test-string gnuplot-double-quoted-escapes
                     "\"double-quoted \\\\ string \\\" with embedded \\\" escapes\"")

(gnuplot-test-string gnuplot-double-quoted-escapes-2
                     "\"escaped quote before closing quote \\\"\"")

(gnuplot-test-string gnuplot-double-quoted-escapes-3
                     "\"escaped backslash before closing quote \\\\\"")

(gnuplot-test-string gnuplot-double-quoted-escapes-4
                     "\"\\\" escaped quote after opening quote\"")

(gnuplot-test-string gnuplot-double-quoted-escapes-5
                     "\"\\\\ escaped backslash after opening quote\"")

(gnuplot-test-string gnuplot-double-quoted-escapes-6
                     "\"\\\\\\\" escaped backslashes + escaped quotes (1) \\\\\\\"\"")

(gnuplot-test-string gnuplot-double-quoted-empty
                     "\"\"")

(gnuplot-test-string gnuplot-double-quoted-string-containing-escaped-quotes
                     "\"\\\"\\\"\"")

(gnuplot-test-string gnuplot-newline-terminated-double-quoted-string
                     "\"newline-terminated
")

(gnuplot-test-string gnuplot-double-quoted-with-embedded-newlines
                     "\"string \\
 with embedded \\
newlines\"")

(gnuplot-test-string gnuplot-newline-terminated-double-quoted-string-with-newline
                     ;; with newlines
                     "\"newline-terminated string \\
 with newlines
")


;;;; Tests for single-quoted strings
(gnuplot-test-string gnuplot-single-quoted-strings
                     "'single-quoted string'")

(gnuplot-test-string gnuplot-single-quoted-empty
                     "''")

(gnuplot-test-string gnuplot-single-quoted-with-double-quotes
                     "'a single-quoted string \"containing a double-quoted string\"'")

(gnuplot-test-string gnuplot-single-quoted-quotes
                     "'embedded '' quote '' characters'")

(gnuplot-test-string gnuplot-single-quoted-quotes-2
                     "'embedded '' quote '' characters'''")

(gnuplot-test-string gnuplot-single-quoted-quotes-3
                     "' '''")

(gnuplot-test-string gnuplot-single-quoted-backslashes
                     "'embedded \\ backslashes \\'")

(gnuplot-test-string gnuplot-single-quoted-backslashes-2
                     "'multiple \\ embedded \\\\ backslashes \\\\\\'")

(gnuplot-test-string gnuplot-single-quoted-trailing-backslash
                     "'trailing backslash\\'")

(gnuplot-test-string gnuplot-single-quoted-newline-terminated
                     "'newline terminated\n")

(gnuplot-test-string gnuplot-single-quoted-newline-terminated-quotes
                     "'embedded '' escapes \\ ending at newline ''\n")

(gnuplot-test-string gnuplot-single-quoted-embedded-newlines
                     "'string \\\n with embedded \\\nnewlines'")

(gnuplot-test-string gnuplot-single-quoted-embedded-newlines-backslashes
                     "'string \\\\\n with \\\\\\\n multiple \\\\\\\\\n backslashes'")

(gnuplot-test-string gnuplot-single-quoted-newline-terminated-embedded-newline
                     "'newline-terminated string \\\n with newlines\n")


;;;; Comment syntax
(eval-and-compile
  (defvar gnuplot-comment-test-contexts
    '("%s"
      "\n%s"
      "\n\n%s\n\n"
      "print 'single-quoted string' %s"
      "print \"double-quoted string\" %s"
      "print 'single-quoted string # with hash mark' %s"
      "print \"double-quoted string # with hash mark\" %s"
      "plot sin(x), cos(x) %s"
      "plot sin(x)
%s
plot cos(x)"
      "# one-line comment
%s"
      "# multi-line \\
comment
%s")
    "List of contexts in which to test syntax recognition of comments."))

(defun gnuplot-test-comment-in-context (comment context)
  "Non-nil if COMMENT is correctly recognised within CONTEXT in gnuplot-mode."
  (cl-destructuring-bind (prologue epilogue)
      (split-string context "%s")
    (with-temp-buffer
      (gnuplot-mode)
      (let (start end)
        (save-excursion
          (insert prologue)
          (setq start (point))
          (insert comment)
          (setq end (point))
          (insert epilogue))
        (syntax-propertize (point-max))
        (goto-char (1+ start))
        (cl-flet ((in-comment-p (position)
                             (nth 4 (syntax-ppss position))))
          (and
           (not (in-comment-p start))
           (cl-loop for position from (1+ start) upto end
                 always (in-comment-p position))
           (or (= end (point-max))
               (not (in-comment-p (1+ end))))))))))

(defmacro gnuplot-test-comment (name comment)
  "Define an `ert' test to check syntax recognition of COMMENT in gnuplot-mode.

The test checks that STRING is correctly recognised as a single
string-literal in multiple different contexts, as determined by
`gnuplot-string-test-contexts'."
  (declare (indent 1))
  `(ert-deftest ,name ()
     ,comment
     ,@(cl-loop for context in gnuplot-comment-test-contexts
             collect
             `(should (gnuplot-test-comment-in-context ,comment ,context)))))

(gnuplot-test-comment gnuplot-comment-simple
                      "# a simple one-line comment")

(gnuplot-test-comment gnuplot-comment-multiline
                      "# a comment\
continued \
over multiple lines")

(gnuplot-test-comment gnuplot-comment-with-hashes
                      "# a comment # with more # hash # characters #")

(gnuplot-test-comment gnuplot-comment-multiline-with-hashes
                      "# a comment \
# continued # over \
mutliple # lines #")

(gnuplot-test-comment gnuplot-comment-with-single-quotes
                      "# a comment 'containing a single-quoted string'")

(gnuplot-test-comment gnuplot-comment-with-single-quotes
                      "# a comment \"containing a double-quoted string\"")

(gnuplot-test-comment gnuplot-comment-multiline-with-quotes
                      "# a continued \
'comment' \
\"containing strings\"")

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gnuplot-tests.el ends here
