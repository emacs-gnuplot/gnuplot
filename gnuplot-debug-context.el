
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

(defmacro with-gnuplot-trace-buffer (&rest body)
  `(with-current-buffer (get-buffer-create "gnuplot-trace")
     ,@body))

(defmacro gnuplot-debug (&rest args)
  `(progn ,@args))

(defmacro gnuplot-trace (&rest args)
  `(with-gnuplot-trace-buffer (insert (format ,@args))))

(defun gnuplot-backtrace ()
  (with-gnuplot-trace-buffer
   (insert "\n-- * backtrace: * --\n\n")
   (dolist (x stack)
     (insert (format "%s\n\n" x)))
   (insert "\n-- end backtrace  --\n\n")))

(defun gnuplot-dump-code (&optional inst)
  (interactive)
  (let ((inst (or inst gnuplot-compiled-grammar)))
    (with-gnuplot-trace-buffer
     (insert "\n-- * compiled code: * --\n\n")
     (dotimes (i (length inst))
       (insert (format "%s\t%s\n" i (aref inst i))))
     (insert "\n--  end compiled code --\n\n")
     (pop-to-buffer (current-buffer)))))

(defun gnuplot-dump-captures ()
  (interactive)
  (with-gnuplot-trace-buffer
   (insert "\n-- * capture groups: * --\n\n")
   (loop for c on gnuplot-captures
	 do
	 (let ((name (caar c))
	       (gnuplot-captures c))
	   (insert (format "%s\t%s\n"
			   name
			   (mapconcat 'gnuplot-token-id
				      (gnuplot-capture-group name)
				      " ")))))
   (insert "\n-- end capture groups  --\n\n")))

(provide 'gnuplot-debug-context)