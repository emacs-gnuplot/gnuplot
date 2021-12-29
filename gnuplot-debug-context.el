;; debugging utilities for the gnuplot-mode context matcher -*- lexical-binding: t -*-

(require 'gnuplot-test-context) ; for gnuplot-simplify-tokens

(defun gnuplot-unload ()
  (interactive)
  (mapatoms
   (lambda (sym)
     (when (string-match
            "gnuplot"
            (symbol-name sym))
       (unintern sym obarray)))))

(defun gnuplot-reload (&optional context)
  (interactive "p")
  (condition-case nil
      (gnuplot-unload)
    (error nil))
  (require 'gnuplot)
  (when context
    (if (= context 16)
        (require 'gnuplot-debug-context))
    (require 'gnuplot-context)))

(defsubst gnuplot-recompile ()
  (save-current-buffer
    (save-window-excursion
      (find-file "gnuplot-context.el")
      (delete-file "gnuplot-context.elc")
      (emacs-lisp-byte-compile)
      (load-file "gnuplot-context.elc"))))

(defun gnuplot-nodebug ()
  (interactive)
  (when (featurep 'gnuplot-debug-context)
    (let ((savef (symbol-function 'gnuplot-debug-on)))
      (unload-feature 'gnuplot-debug-context)
      (fset 'gnuplot-debug-on savef)))
  (gnuplot-recompile))

(defun gnuplot-debug-on ()
  (interactive)
  (unless (featurep 'gnuplot-debug-context)
    (load-library "gnuplot-debug-context"))
  (gnuplot-recompile))

(defmacro with-gnuplot-trace-buffer (&rest body)
  `(with-current-buffer (get-buffer-create "gnuplot-trace")
     ,@body))

(defmacro gnuplot-debug (&rest args)
  `(progn ,@args))

(defmacro gnuplot-trace (&rest args)
  `(with-gnuplot-trace-buffer (insert (format ,@args))))

(defun gnuplot-backtrace (stack)
  (if stack
      (with-gnuplot-trace-buffer
       (insert "\n-- * backtrace: * --\n")
       (dolist (x stack)
         (insert (format "%s\n"
                         (if (eq (car x) 'return)
                             x
                           (list (car x) (cadr x)
                                 (gnuplot-simplify-tokens (cl-caddr x)))))))
       (insert "-- end backtrace  --\n"))))

(defun gnuplot-dump-backtrack (backtrack)
  (if backtrack
      (with-gnuplot-trace-buffer
       (insert "\n-- * backtrack records: * --\n")
       (dolist (x backtrack)
         (insert (format "%s\t%s\n" (cl-caddr x) (gnuplot-simplify-tokens (cadr x)))))
       (insert "-- end backtrack records  --\n\n"))))

(defun gnuplot-dump-progress (progress)
  (if progress
      (with-gnuplot-trace-buffer
       (insert "\n-- * progress records: * --\n")
       (dolist (x progress)
         (insert (format "%s\t%s\n" (car x) (gnuplot-simplify-tokens (cdr x)))))
       (insert "-- end progress records  --\n\n"))))

(defun gnuplot-dump-code (&optional inst)
  (interactive)
  (let ((inst (or inst gnuplot-compiled-grammar)))
    (with-gnuplot-trace-buffer
     (insert "\n-- * compiled code: * --\n")
     (dotimes (i (length inst))
       (insert (format "%s\t%s\n" i (aref inst i))))
     (insert "--  end compiled code --\n\n")
     (pop-to-buffer (current-buffer)))))

(defun gnuplot-dump-captures ()
  (interactive)
  (if gnuplot-captures
      (with-gnuplot-trace-buffer
       (insert "\n-- * capture groups: * --\n")
       (cl-loop for c on gnuplot-captures
             do
             (let ((name (caar c))
                   (gnuplot-captures c))
               (insert (format "%s\t%s\n"
                               name
                               (mapconcat 'gnuplot-token-id
                                          (gnuplot-capture-group name)
                                          " ")))))
       (insert "-- end capture groups  --\n\n"))))

(provide 'gnuplot-debug-context)

(gnuplot-debug-on)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gnuplot-debug-context.el ends here
