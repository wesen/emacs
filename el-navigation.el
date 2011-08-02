;; From: Helmut Eller <eller.helmut@gmail.com>
;; Newsgroups: gnu.emacs.help
;; Date: Tue, 22 Sep 2009 11:21:31 +0200
;; Message-ID: <m2tyyvcp2c.fsf@gmail.com>
;; Xref: news.stanford.edu gnu.emacs.help:173250
;; To: help-gnu-emacs@gnu.org
;; Subject: Re: Go to Emacs-Lisp Definition
;; 
;; * Nordlöw [2009-09-22 11:13+0200] writes:
;; 
;; > Has anyone implemented a go to (lookup) definition of an Emacs-Lisp
;; > symbol: function, variable, face, etc kind of like find-tag but using
;; > Emacs own symbol database (hash-table)?
;; >
;; > I know that I can achieve this in Vanilla Emacs with C-h f,v, but
;; > direct jump (M-.) would be more user efficient!
;; 
;; I have this in my .emacs.  It's some random elisp hacking stuff.
;; Pick what you need.
;; 
;; Helmut

(defun elisp-disassemble (function)
  (interactive (list (function-called-at-point)))
  (disassemble function))

(defun elisp-pp (sexp)
  (with-output-to-temp-buffer "*Pp Eval Output*"
    (pp sexp)
    (with-current-buffer standard-output
      (emacs-lisp-mode))))

(defun elisp-macroexpand (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (macroexpand form)))

(defun elisp-macroexpand-all (form)
  (interactive (list (form-at-point 'sexp)))
  (elisp-pp (cl-macroexpand-all form)))

(defun elisp-push-point-marker ()
  (require 'etags)
  (cond ((featurep 'xemacs)
         (push-tag-mark))
        (t (ring-insert find-tag-marker-ring (point-marker)))))

(defun elisp-pop-found-function ()
  (interactive)
  (cond ((featurep 'xemacs) (pop-tag-mark nil))
        (t (pop-tag-mark))))

(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
	 (let ((symbol (intern-soft name))
	       (search (lambda (fun sym)
			 (let* ((r (save-excursion (funcall fun sym)))
				(buffer (car r))
				(point (cdr r)))
			   (cond ((not point)
				  (error "Found no definition for %s in %s"
					 name buffer))
				 (t
				  (switch-to-buffer buffer)
				  (goto-char point)
				  (recenter 1)))))))
	   (cond ((fboundp symbol)
		  (elisp-push-point-marker)
		  (funcall search 'find-function-noselect symbol))
		 ((boundp symbol)
		  (elisp-push-point-marker)
		  (funcall search 'find-variable-noselect symbol))
		 (t
		  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))

(defun elisp-bytecompile-and-load ()
  (interactive)
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  (require 'bytecomp)
  ;; Recompile if file or buffer has changed since last compilation.
  (when  (and (buffer-modified-p)
	      (y-or-n-p (format "save buffer %s first? " (buffer-name))))
      (save-buffer))
  (let ((filename (expand-file-name buffer-file-name)))
    (with-temp-buffer
      (byte-compile-file filename t))))

(defvar elisp-extra-keys 
  '(((kbd "C-c d")   'elisp-disassemble)
    ((kbd "C-c m")   'elisp-macroexpand)
    ((kbd "C-c M")   'elisp-macroexpand-all)
    ((kbd "C-c C-c") 'compile-defun)
    ((kbd "C-c C-k") 'elisp-bytecompile-and-load)
    ((kbd "C-c C-l") 'load-file)
    ((kbd "C-c p")   'pp-eval-last-sexp)
    ((kbd "M-.")     'elisp-find-definition)
    ((kbd "M-,")     'elisp-pop-found-function)
    ((kbd "C-c <")   'list-callers)))

(dolist (binding elisp-extra-keys)
  (let ((key (eval (car binding))) (val (eval (cadr binding))))
    (define-key emacs-lisp-mode-map key val)
    (define-key lisp-interaction-mode-map key val)))

(provide 'el-navigation)