;;; Interval timers for GNU Emacs
;;; Copyright (C) 1988, 1991, 1993, 1997, 1998 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle_jones@wonderworks.com

;(provide 'itimer)

(require 'lisp-float-type)

;; `itimer' feature means Emacs-Lisp programmers get:
;;    itimerp
;;    itimer-live-p
;;    itimer-value
;;    itimer-restart
;;    itimer-function
;;    itimer-uses-arguments
;;    itimer-function-arguments
;;    set-itimer-value
;;    set-itimer-restart
;;    set-itimer-function
;;    set-itimer-uses-arguments
;;    set-itimer-function-arguments
;;    get-itimer
;;    start-itimer
;;    read-itimer
;;    delete-itimer
;;    activate-itimer
;;
;; Interactive users get these commands:
;;    edit-itimers
;;    list-itimers
;;    start-itimer
;;
;; See the doc strings of these functions for more information.

(defconst itimer-version "1.09"
  "Version number of the itimer package.")

(defvar itimer-list nil
  "List of all active itimers.")

(defvar itimer-process nil
  "Process that drives all itimers, if a subprocess is being used.")

(defvar itimer-timer nil
  "Emacs internal timer that drives the itimer system, if a subprocess
is not being used to drive the system.")

(defvar itimer-timer-last-wakeup nil
  "The time the timer driver function last ran.")

(defvar itimer-short-interval 1e-3
  "Interval used for scheduling an event a very short time in the future.
Used internally to make the scheduler wake up early.
Unit is seconds.")

;; This value is maintained internally; it does not determine
;; itimer granularity.  Itimer granularity is 1 second if your
;; Emacs doesn't support floats or your system doesn't have a
;; clock with microsecond granularity.  Otherwise granularity is
;; to the microsecond, although you can't possibly get timers to be
;; executed with this kind of accuracy in practice.  There will
;; be delays due to system and Emacs internal activity that delay
;; dealing with synchronous events and process output.
(defvar itimer-next-wakeup itimer-short-interval
  "Itimer process will wakeup to service running itimers within this
many seconds.")

(defvar itimer-edit-map nil
  "Keymap used when in Itimer Edit mode.")

(if itimer-edit-map
    ()
  (setq itimer-edit-map (make-sparse-keymap))
  (define-key itimer-edit-map "s" 'itimer-edit-set-field)
  (define-key itimer-edit-map "d" 'itimer-edit-delete-itimer)
  (define-key itimer-edit-map "q" 'itimer-edit-quit)
  (define-key itimer-edit-map "\t" 'itimer-edit-next-field)
  (define-key itimer-edit-map " " 'next-line)
  (define-key itimer-edit-map "n" 'next-line)
  (define-key itimer-edit-map "p" 'previous-line)
  (define-key itimer-edit-map "\C-?" 'itimer-edit-previous-field)
  (define-key itimer-edit-map "x" 'start-itimer)
  (define-key itimer-edit-map "?" 'itimer-edit-help))

(defvar itimer-inside-driver nil)

(defvar itimer-edit-start-marker nil)

;; macros must come first... or byte-compile'd code will throw back its
;; head and scream.

(defmacro itimer-decrement (variable)
  (list 'setq variable (list '1- variable)))

(defmacro itimer-increment (variable)
  (list 'setq variable (list '1+ variable)))

(defmacro itimer-signum (n)
  (list 'if (list '> n 0) 1
    (list 'if (list 'zerop n) 0 -1)))

;; Itimer access functions should behave as if they were subrs.  These
;; macros are used to check the arguments to the itimer functions and
;; signal errors appropriately if the arguments are not valid.

(defmacro check-itimer (var)
  "If VAR is not bound to an itimer, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'itimerp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''itimerp var)))))

(defmacro check-itimer-coerce-string (var)
  "If VAR is not bound to a string, look up the itimer that it names and
bind VAR to it.  Otherwise, if VAR is not bound to an itimer, signal
wrong-type-argument.  This is a macro."
  (list 'setq var
	(list 'cond
	      (list (list 'itimerp var) var)
	      (list (list 'stringp var) (list 'get-itimer var))
	      (list t (list 'signal ''wrong-type-argument
			    (list 'list ''string-or-itimer-p var))))))

(defmacro check-nonnegative-number (var)
  "If VAR is not bound to a number, signal wrong-type-argument.
If VAR is not bound to a positive number, signal args-out-of-range.
This is a macro."
  (list 'setq var
	(list 'if (list 'not (list 'numberp var))
	      (list 'signal ''wrong-type-argument
		    (list 'list ''natnump var))
	      (list 'if (list '< var 0)
		    (list 'signal ''args-out-of-range (list 'list var))
		    var))))

(defmacro check-string (var)
  "If VAR is not bound to a string, signal wrong-type-argument.
This is a macro."
  (list 'setq var
	(list 'if (list 'stringp var) var
	      (list 'signal ''wrong-type-argument
		    (list 'list ''stringp var)))))

;; Functions to access and modify itimer attributes.

(defun itimerp (obj)
  "Return t if OBJ is an itimer."
  (and (consp obj) (eq (length obj) 8)))

(defun itimer-live-p (obj)
  "Return non-nil if OBJ is an itimer and is active.
``Active'' means Emacs will run it when it expires.
`activate-timer' must be called on an itimer to make it active.
Itimers started with `start-itimer' are automatically active."
  (and (itimerp obj) (memq obj itimer-list)))

(defun itimer-name (itimer)
  "Return the name of ITIMER."
  (check-itimer itimer)
  (car itimer))

(defun itimer-value (itimer)
  "Return the number of seconds until ITIMER expires."
  (check-itimer itimer)
  (nth 1 itimer))

(defun itimer-restart (itimer)
  "Return the value to which ITIMER will be set at restart.
Return nil if this itimer doesn't restart."
  (check-itimer itimer)
  (nth 2 itimer))

(defun itimer-function (itimer)
  "Return the function of ITIMER.
This function is called each time ITIMER expires."
  (check-itimer itimer)
  (nth 3 itimer))

(defun itimer-is-idle (itimer)
  "Return non-nil if ITIMER is an idle timer.
Normal timers expire after a set interval.  Idle timers expire
only after Emacs has been idle for a specific interval.
``Idle'' means no command events occur within the interval."
  (check-itimer itimer)
  (nth 4 itimer))

(defun itimer-uses-arguments (itimer)
  "Return non-nil if the function of ITIMER will be called with arguments.
ITIMER's function is called with the arguments each time ITIMER expires.
The arguments themselves are retrievable with `itimer-function-arguments'."
  (check-itimer itimer)
  (nth 5 itimer))

(defun itimer-function-arguments (itimer)
  "Return the function arguments of ITIMER as a list.
ITIMER's function is called with these argument each time ITIMER expires."
  (check-itimer itimer)
  (nth 6 itimer))

(defun itimer-recorded-run-time (itimer)
  (check-itimer itimer)
  (nth 7 itimer))

(defun set-itimer-value (itimer value)
  "Set the timeout value of ITIMER to be VALUE.
Itimer will expire in this many seconds.
If your version of Emacs supports floating point numbers then
VALUE can be a floating point number.  Otherwise it
must be an integer.
Returns VALUE."
  (check-itimer itimer)
  (check-nonnegative-number value)
  (let ((inhibit-quit t))
    ;; If the itimer is in the active list, and under the new
    ;; timeout value would expire before we would normally
    ;; wakeup, wakeup now and recompute a new wakeup time.
    (or (and (< value itimer-next-wakeup)
	     (and (itimer-name itimer) (get-itimer (itimer-name itimer)))
	     (progn (itimer-driver-wakeup)
		    (setcar (cdr itimer) value)
		    (itimer-driver-wakeup)
		    t ))
	(setcar (cdr itimer) value))
    value))

;; Same as set-itimer-value but does not wakeup the driver.
;; Only should be used by the drivers when processing expired timers.
(defun set-itimer-value-internal (itimer value)
  (check-itimer itimer)
  (check-nonnegative-number value)
  (setcar (cdr itimer) value))

(defun set-itimer-restart (itimer restart)
  "Set the restart value of ITIMER to be RESTART.
If RESTART is nil, ITIMER will not restart when it expires.
If your version of Emacs supports floating point numbers then
RESTART can be a floating point number.  Otherwise it
must be an integer.
Returns RESTART."
  (check-itimer itimer)
  (if restart (check-nonnegative-number restart))
  (setcar (cdr (cdr itimer)) restart))

(defun set-itimer-function (itimer function)
  "Set the function of ITIMER to be FUNCTION.
FUNCTION will be called when itimer expires.
Returns FUNCTION."
  (check-itimer itimer)
  (setcar (nthcdr 3 itimer) function))

(defun set-itimer-is-idle (itimer flag)
  "Set flag that says whether ITIMER is an idle timer.
If FLAG is non-nil, then ITIMER will be considered an idle timer.
Returns FLAG."
  (check-itimer itimer)
  (setcar (nthcdr 4 itimer) flag))

(defun set-itimer-uses-arguments (itimer flag)
  "Set flag that says whether the function of ITIMER is called with arguments.
If FLAG is non-nil, then the function will be called with one argument,
otherwise the function will be called with no arguments.
Returns FLAG."
  (check-itimer itimer)
  (setcar (nthcdr 5 itimer) flag))

(defun set-itimer-function-arguments (itimer &optional arguments)
  "Set the function arguments of ITIMER to be ARGUMENTS.
The function of ITIMER will be called with ARGUMENTS when itimer expires.
Returns ARGUMENTS."
  (check-itimer itimer)
  (setcar (nthcdr 6 itimer) arguments))

(defun set-itimer-recorded-run-time (itimer time)
  (check-itimer itimer)
  (setcar (nthcdr 7 itimer) time))

(defun get-itimer (name)
  "Return itimer named NAME, or nil if there is none."
  (check-string name)
  (assoc name itimer-list))

(defun read-itimer (prompt &optional initial-input)
  "Read the name of an itimer from the minibuffer and return the itimer
associated with that name.  The user is prompted with PROMPT.
Optional second arg INITIAL-INPUT non-nil is inserted into the
minibuffer as initial user input."
  (get-itimer (completing-read prompt itimer-list nil 'confirm initial-input)))

(defun delete-itimer (itimer)
  "Delete ITIMER.  ITIMER may be an itimer or the name of one."
  (check-itimer-coerce-string itimer)
  (setq itimer-list (delq itimer itimer-list)))

(defun start-itimer (name function value &optional restart
		     is-idle with-args &rest function-arguments)
  "Start an itimer.
Arguments are
  NAME, FUNCTION, VALUE &optional RESTART, IS-IDLE, WITH-ARGS, &rest FUNCTION-ARGUMENTS.
NAME is an identifier for the itimer.  It must be a string.  If an itimer
  already exists with this name, NAME will be modified slightly to make
  it unique.
FUNCTION should be a function (or symbol naming one).  It
  will be called each time the itimer expires with arguments of
  FUNCTION-ARGUMENTS.  The function can access the itimer that
  invoked it through the variable `current-itimer'.  If WITH-ARGS
  is nil then FUNCTION is called with no arguments.  This is for
  backward compatibility with older versions of the itimer
  package which always called FUNCTION with no arguments.
VALUE is the number of seconds until this itimer expires.
  If your version of Emacs supports floating point numbers then
  VALUE can be a floating point number.  Otherwise it
  must be an integer.
Optional fourth arg RESTART non-nil means that this itimer should be
  restarted automatically after its function is called.  Normally an itimer
  is deleted at expiration after its function has returned.
  If non-nil, RESTART should be a number indicating the value at which
  the itimer should be set at restart time.
Optional fifth arg IS-IDLE specifies if this is an idle timer.
  Normal timers expire after a set interval.  Idle timers expire
  only after Emacs has been idle for specific interval.
  ``Idle'' means no command events occur within the interval.
Returns the newly created itimer."
  (interactive
   (list (completing-read "Start itimer: " itimer-list)
	 (read (completing-read "Itimer function: " obarray 'fboundp))
	 (let (value)
	   (while (or (not (numberp value)) (< value 0))
	     (setq value (read-from-minibuffer "Itimer value: " nil nil t)))
	   value)
	 (let ((restart t))
	   (while (and restart (or (not (numberp restart)) (< restart 0)))
	     (setq restart (read-from-minibuffer "Itimer restart: "
						 nil nil t)))
	   restart)
	 ;; hard to imagine the user specifying these interactively
	 nil
	 nil ))
  (check-string name)
  (check-nonnegative-number value)
  (if restart (check-nonnegative-number restart))
  ;; Make proposed itimer name unique if it's not already.
  (let ((oname name)
	(num 2))
    (while (get-itimer name)
      (setq name (format "%s<%d>" oname num))
      (itimer-increment num)))
  (activate-itimer (list name value restart function is-idle
			 with-args function-arguments (list 0 0 0)))
  (car itimer-list))

(defun make-itimer ()
  "Create an unactivated itimer.
The itimer will not begin running until activated with `activate-itimer'.
Set the itimer's expire interval with `set-itimer-value'.
Set the itimer's function interval with `set-itimer-function'.
Once this is done, the timer can be activated."
  (list nil 0 nil 'ignore nil nil nil (list 0 0 0)))

(defun activate-itimer (itimer)
  "Activate ITIMER, which was previously created with `make-itimer'.
ITIMER will be added to the global list of running itimers,
its FUNCTION will be called when it expires, and so on."
  (check-itimer itimer)
  (if (memq itimer itimer-list)
      (error "itimer already activated"))
  (if (not (numberp (itimer-value itimer)))
      (error "itimer timeout value not a number: %s" (itimer-value itimer)))
  (if (<= (itimer-value itimer) 0)
      (error "itimer timeout value not positive: %s" (itimer-value itimer)))
  ;; If there's no itimer driver/process, start one now.
  ;; Otherwise wake up the itimer driver so that seconds slept before
  ;; the new itimer is created won't be counted against it.
  (if (or itimer-process itimer-timer)
      (itimer-driver-wakeup)
    (itimer-driver-start))
  ;; Roll a unique name for the timer if it doesn't have a name
  ;; already.
  (if (not (stringp (car itimer)))
      (let ((name "itimer-0")
	    (oname "itimer-")
	    (num 1))
	(while (get-itimer name)
	  (setq name (format "%s<%d>" oname num))
	  (itimer-increment num))
	(setcar itimer name))
    ;; signal an error if the timer's name matches an already
    ;; activated timer.
    (if (get-itimer (itimer-name itimer))
	(error "itimer named \"%s\" already existing and activated"
	       (itimer-name itimer))))
  (let ((inhibit-quit t))
    ;; add the itimer to the global list
    (setq itimer-list (cons itimer itimer-list))
    ;; If the itimer process is scheduled to wake up too late for
    ;; the itimer we wake it up to calculate a correct wakeup
    ;; value giving consideration to the newly added itimer.
    (if (< (itimer-value itimer) itimer-next-wakeup)
	(itimer-driver-wakeup))))

;; User level functions to list and modify existing itimers.
;; Itimer Edit major mode, and the editing commands thereof.

(defun list-itimers ()
  "Pop up a buffer containing a list of all itimers.
The major mode of the buffer is Itimer Edit mode.  This major mode provides
commands to manipulate itimers; see the documentation for
`itimer-edit-mode' for more information."
  (interactive)
  (let* ((buf (get-buffer-create "*Itimer List*"))
	 (opoint (point))
	 (standard-output buf)
	 (itimers (reverse itimer-list)))
    (set-buffer buf)
    (itimer-edit-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert
"Name                  Value   Restart   Function            Idle   Arguments"
"\n"
"----                  -----   -------   --------            ----   --------")
    (if (null itimer-edit-start-marker)
	(setq itimer-edit-start-marker (point)))
    (while itimers
      (newline 1)
      (prin1 (itimer-name (car itimers)))
      (tab-to-tab-stop)
      (insert (itimer-truncate-string
	       (format "%5.5s" (itimer-value (car itimers))) 5))
      (tab-to-tab-stop)
      (insert (itimer-truncate-string
	       (format "%5.5s" (itimer-restart (car itimers))) 5))
      (tab-to-tab-stop)
      (insert (itimer-truncate-string
	       (format "%.19s" (itimer-function (car itimers))) 19))
      (tab-to-tab-stop)
      (if (itimer-is-idle (car itimers))
	  (insert "yes")
	(insert "no"))
      (tab-to-tab-stop)
      (if (itimer-uses-arguments (car itimers))
	  (prin1 (itimer-function-arguments (car itimers)))
	(prin1 'NONE))
      (setq itimers (cdr itimers)))
    ;; restore point
    (goto-char opoint)
    (if (< (point) itimer-edit-start-marker)
	(goto-char itimer-edit-start-marker))
    (setq buffer-read-only t)
    (display-buffer buf)))

(defun edit-itimers ()
  "Display a list of all itimers and select it for editing.
The major mode of the buffer containing the listing is Itimer Edit mode.
This major mode provides commands to manipulate itimers; see the documentation
for `itimer-edit-mode' for more information."
  (interactive)
  ;; since user is editing, make sure displayed data is reasonably up-to-date
  (if (or itimer-process itimer-timer)
      (itimer-driver-wakeup))
  (list-itimers)
  (select-window (get-buffer-window "*Itimer List*"))
  (goto-char itimer-edit-start-marker)
  (if itimer-list
      (progn
	(forward-sexp 2)
	(backward-sexp)))
  (message "type q to quit, ? for help"))

;; no point in making this interactive.
(defun itimer-edit-mode ()
  "Major mode for manipulating itimers.
Attributes of running itimers are changed by moving the cursor to the
desired field and typing `s' to set that field.  The field will then be
set to the value read from the minibuffer.

Commands:
TAB    move forward a field
DEL    move backward a field
s      set a field
d      delete the selected itimer
x      start a new itimer
?      help"
  (kill-all-local-variables)
  (make-local-variable 'tab-stop-list)
  (setq major-mode 'itimer-edit-mode
	mode-name "Itimer Edit"
	truncate-lines t
	tab-stop-list '(22 32 40 60 67))
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (buffer-flush-undo (current-buffer))
  (use-local-map itimer-edit-map)
  (set-syntax-table emacs-lisp-mode-syntax-table))

(put 'itimer-edit-mode 'mode-class 'special)

(defun itimer-edit-help ()
  "Help function for Itimer Edit."
  (interactive)
  (if (eq last-command 'itimer-edit-help)
      (describe-mode)
    (message "TAB, DEL select fields, (s)et field, (d)elete itimer   (type ? for more help)")))

(defun itimer-edit-quit ()
  "End Itimer Edit."
  (interactive)
  (bury-buffer (current-buffer))
  (if (one-window-p t)
      (switch-to-buffer (other-buffer (current-buffer)))
    (delete-window)))

(defun itimer-edit-set-field ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) itimer-edit-start-marker)
      (error ""))
  ;; field-value must be initialized to be something other than a
  ;; number, symbol, or list.
  (let (itimer field (field-value ""))
    (setq itimer (save-excursion
		  ;; read the name of the itimer from the beginning of
		  ;; the current line.
		  (beginning-of-line)
		  (get-itimer (read (current-buffer))))
	  field (save-excursion
		  (itimer-edit-beginning-of-field)
		  (let ((opoint (point))
			(n 0))
		    ;; count the number of sexprs until we reach the cursor
		    ;; and use this info to determine which field the user
		    ;; wants to modify.
		    (beginning-of-line)
		    (while (and (>= opoint (point)) (< n 6))
		      (forward-sexp 2)
		      (backward-sexp)
		      (itimer-increment n))
		    (cond ((eq n 1) (error "Cannot change itimer name."))
			  ((eq n 2) 'value)
			  ((eq n 3) 'restart)
			  ((eq n 4) 'function)
			  ((eq n 5) 'is-idle)
			  (t 'function-argument)))))
    (cond ((eq field 'value)
	   (let ((prompt "Set itimer value: "))
	     (while (not (natnump field-value))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'restart)
	   (let ((prompt "Set itimer restart: "))
	     (while (and field-value (not (natnump field-value)))
	       (setq field-value (read-from-minibuffer prompt nil nil t)))))
	  ((eq field 'function)
	   (let ((prompt "Set itimer function: "))
	     (while (not (or (and (symbolp field-value) (fboundp field-value))
			     (and (consp field-value)
				  (memq (car field-value) '(lambda macro)))))
	       (setq field-value
		     (read (completing-read prompt obarray 'fboundp nil))))))
	  ((eq field 'is-idle)
	   (setq field-value (not (itimer-is-idle itimer))))
	  ((eq field 'function-argument)
	   (let ((prompt "Set itimer function argument: "))
	     (setq field-value (read-expression prompt))
	     (cond ((not (listp field-value))
		    (setq field-value (list field-value))))
	     (if (null field-value)
		 (set-itimer-uses-arguments itimer nil)
	       (set-itimer-uses-arguments itimer t)))))
    ;; set the itimer field
    (funcall (intern (concat "set-itimer-" (symbol-name field)))
	     itimer field-value)
    ;; move to beginning of field to be changed
    (itimer-edit-beginning-of-field)
    ;; modify the list buffer to reflect the change.
    (let (buffer-read-only kill-ring)
      (kill-sexp 1)
      (kill-region (point) (progn (skip-chars-forward " \t") (point)))
      (prin1 field-value (current-buffer))
      (if (not (eolp))
	  (tab-to-tab-stop))
      (backward-sexp))))

(defun itimer-edit-delete-itimer ()
  (interactive)
  ;; First two lines in list buffer are headers.
  ;; Cry out against the luser who attempts to change a field there.
  (if (<= (point) itimer-edit-start-marker)
      (error ""))
  (delete-itimer
   (read-itimer "Delete itimer: "
	       (save-excursion (beginning-of-line) (read (current-buffer)))))
  ;; update list information
  (list-itimers))

(defun itimer-edit-next-field (count)
  (interactive "p")
  (itimer-edit-beginning-of-field)
  (cond ((> (itimer-signum count) 0)
	 (while (not (zerop count))
	   (forward-sexp)
	   ;; wrap from eob to itimer-edit-start-marker
	   (if (eobp)
	       (progn
		 (goto-char itimer-edit-start-marker)
		 (forward-sexp)))
	   (forward-sexp)
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (progn
		 (forward-sexp 2)
		 (backward-sexp)))
	   (itimer-decrement count)))
	((< (itimer-signum count) 0)
	 (while (not (zerop count))
	   (backward-sexp)
	   ;; treat fields at beginning of line as if they weren't there.
	   (if (bolp)
	       (backward-sexp))
	   ;; wrap from itimer-edit-start-marker to field at eob.
	   (if (<= (point) itimer-edit-start-marker)
	       (progn
		 (goto-char (point-max))
		 (backward-sexp)))
	   (itimer-increment count)))))

(defun itimer-edit-previous-field (count)
  (interactive "p")
  (itimer-edit-next-field (- count)))

(defun itimer-edit-beginning-of-field ()
  (let ((forw-back (save-excursion (forward-sexp) (backward-sexp) (point)))
	(back (save-excursion (backward-sexp) (point))))
    (cond ((eq forw-back back) (backward-sexp))
	  ((eq forw-back (point)) t)
	  (t (backward-sexp)))))

(defun itimer-truncate-string (str len)
  (if (<= (length str) len)
      str
    (substring str 0 len)))

;; internals of the itimer implementation.

(defun itimer-run-expired-timers (time-elapsed)
  (let ((itimers (copy-sequence itimer-list))
	(itimer)
	(next-wakeup 600)
	(idle-time)
	(last-event-time)
	(recorded-run-time)
	;; process filters can be hit by stray C-g's from the user,
	;; so we must protect this stuff appropriately.
	;; Quit's are allowed from within itimer functions, but we
	;; catch them and print a message.
	(inhibit-quit t))
    (setq next-wakeup 600)
    (cond ((and (boundp 'last-command-event-time)
		(consp last-command-event-time))
	   (setq last-event-time last-command-event-time
		 idle-time (itimer-time-difference (current-time)
						   last-event-time)))
	  ((and (boundp 'last-input-time) (consp last-input-time))
	   (setq last-event-time (list (car last-input-time)
				       (cdr last-input-time)
				       0)
		 idle-time (itimer-time-difference (current-time)
						   last-event-time)))
	  ;; no way to do this under FSF Emacs yet.
	  (t (setq last-event-time '(0 0 0)
		   idle-time 0)))
    (while itimers
      (setq itimer (car itimers))
      (if (itimer-is-idle itimer)
	  (setq recorded-run-time (itimer-recorded-run-time itimer))
	(set-itimer-value-internal itimer (max 0 (- (itimer-value itimer)
						    time-elapsed))))
      (if (if (itimer-is-idle itimer)
	      (or (> (itimer-time-difference recorded-run-time
					     last-event-time)
		     0)
		  (< idle-time (itimer-value itimer)))
	    (> (itimer-value itimer) 0))
	  (setq next-wakeup
		(if (itimer-is-idle itimer)
		    (if (< idle-time (itimer-value itimer))
			(min next-wakeup (- (itimer-value itimer) idle-time))
		      (min next-wakeup (itimer-value itimer)))
		  (min next-wakeup (itimer-value itimer))))
	(and (itimer-is-idle itimer)
	     (set-itimer-recorded-run-time itimer (current-time)))
	;; itimer has expired, we must call its function.
	;; protect our local vars from the itimer function.
	;; allow keyboard quit to occur, but catch and report it.
	;; provide the variable `current-itimer' in case the function
	;; is interested.
	(unwind-protect
	    (condition-case condition-data
		(save-match-data
		  (let* ((current-itimer itimer)
			 (quit-flag nil)
			 (inhibit-quit nil)
			 ;; for FSF Emacs timer.el emulation under XEmacs.
			 ;; eldoc expect this to be done, apparently.
			 (this-command nil)
			 itimer itimers time-elapsed)
		    (if (itimer-uses-arguments current-itimer)
			(apply (itimer-function current-itimer)
			       (itimer-function-arguments current-itimer))
		      (funcall (itimer-function current-itimer)))))
	      (error (message "itimer \"%s\" signaled: %s" (itimer-name itimer)
			      (prin1-to-string condition-data)))
	      (quit (message "itimer \"%s\" quit" (itimer-name itimer))))
	  ;; restart the itimer if we should, otherwise delete it.
	  (if (null (itimer-restart itimer))
	      (delete-itimer itimer)
	    (set-itimer-value-internal itimer (itimer-restart itimer))
	    (setq next-wakeup (min next-wakeup (itimer-value itimer))))))
      (setq itimers (cdr itimers)))
    ;; make another sweep through the list to catch any timers
    ;; that might have been added by timer functions above.
    (setq itimers itimer-list)
    (while itimers
      (setq next-wakeup (min next-wakeup (itimer-value (car itimers)))
	    itimers (cdr itimers)))
    ;; if user is viewing the timer list, update displayed info.
    (let ((b (get-buffer "*Itimer List*")))
      (if (and b (get-buffer-window b))
	  (save-excursion
	    (list-itimers))))
    next-wakeup ))

(defun itimer-process-filter (process string)
  ;; If the itimer process dies and generates output while doing
  ;; so, we may be called before the process-sentinel.  Sanity
  ;; check the output just in case...
  (if (not (string-match "^[0-9]" string))
      (progn (message "itimer process gave odd output: %s" string)
	     ;; it may be still alive and waiting for input
	     (process-send-string itimer-process "3\n"))
    ;; if there are no active itimers, return quickly.
    (if itimer-list
	(let ((wakeup nil))
	  (unwind-protect
	      (setq wakeup (itimer-run-expired-timers (string-to-int string)))
	    (and (null wakeup) (process-send-string process "1\n")))
	  (setq itimer-next-wakeup wakeup))
      (setq itimer-next-wakeup 600))
    ;; tell itimer-process when to wakeup again
    (process-send-string itimer-process
			 (concat (int-to-string itimer-next-wakeup)
				 "\n"))))

(defun itimer-process-sentinel (process message)
  (let ((inhibit-quit t))
    (if (eq (process-status process) 'stop)
	(continue-process process)
      ;; not stopped, so it must have died.
      ;; cleanup first...
      (delete-process process)
      (setq itimer-process nil)
      ;; now, if there are any active itimers then we need to immediately
      ;; start another itimer process, otherwise we can wait until the next
      ;; start-itimer call, which will start one automatically.
      (if (null itimer-list)
	  ()
	;; there may have been an error message in the echo area;
	;; give the user at least a little time to read it.
	(sit-for 2)
	(message "itimer process %s... respawning." (substring message 0 -1))
	(itimer-process-start)))))

(defun itimer-process-start ()
  (let ((inhibit-quit t)
	(process-connection-type nil))
    (setq itimer-process (start-process "itimer" nil "itimer"))
    (process-kill-without-query itimer-process)
    (set-process-filter itimer-process 'itimer-process-filter)
    (set-process-sentinel itimer-process 'itimer-process-sentinel)
    ;; Tell itimer process to wake up quickly, so that a correct
    ;; wakeup time can be computed.  Zero loses because of
    ;; underlying itimer implementations that use 0 to mean
    ;; `disable the itimer'.
    (setq itimer-next-wakeup itimer-short-interval)
    (process-send-string itimer-process
			 (format "%s\n" itimer-next-wakeup))))

(defun itimer-process-wakeup ()
  (interrupt-process itimer-process)
  (accept-process-output))

(defun itimer-timer-start ()
  (let ((inhibit-quit t))
    (setq itimer-next-wakeup itimer-short-interval
	  itimer-timer-last-wakeup (current-time)
	  itimer-timer (add-timeout itimer-short-interval
				    'itimer-timer-driver nil nil))))

(defun itimer-disable-timeout (timeout)
  ;; Disgusting hack, but necessary because there is no other way
  ;; to remove a timer that has a restart value from while that
  ;; timer's function is being run.  (FSF Emacs only.)
  (if (vectorp timeout)
      (aset timeout 4 nil))
  (disable-timeout timeout))

(defun itimer-timer-wakeup ()
  (let ((inhibit-quit t))
    (itimer-disable-timeout itimer-timer)
    (setq itimer-timer (add-timeout itimer-short-interval
				    'itimer-timer-driver nil 5))))

(defun itimer-time-difference (t1 t2)
  (let (usecs secs 65536-secs carry)
    (setq usecs (- (nth 2 t1) (nth 2 t2)))
    (if (< usecs 0)
	(setq carry 1
	      usecs (+ usecs 1000000))
      (setq carry 0))
    (setq secs (- (nth 1 t1) (nth 1 t2) carry))
    (if (< secs 0)
	 (setq carry 1
	       secs (+ secs 65536))
      (setq carry 0))
    (setq 65536-secs (- (nth 0 t1) (nth 0 t2) carry))
    (+ (* 65536-secs 65536.0)
       secs
       (/ usecs 1000000.0))))

(defun itimer-timer-driver (&rest ignored)
  ;; inhibit quit because if the user quits at an inopportune
  ;; time, the timer process won't be launched again and the
  ;; system stops working.  itimer-run-expired-timers allows
  ;; individual timer function to be aborted, so the user can
  ;; escape a feral timer function.
  (if (not itimer-inside-driver)
      (let* ((inhibit-quit t)
	     (itimer-inside-driver t)
	     (now (current-time))
	     (elapsed (itimer-time-difference now itimer-timer-last-wakeup))
	     (sleep nil))
	(setq itimer-timer-last-wakeup now
	      sleep (itimer-run-expired-timers elapsed))
	(itimer-disable-timeout itimer-timer)
	(setq itimer-next-wakeup sleep
	      itimer-timer (add-timeout sleep 'itimer-timer-driver nil 5)))))

(defun itimer-driver-start ()
  (if (fboundp 'add-timeout)
      (itimer-timer-start)
    (itimer-process-start)))

(defun itimer-driver-wakeup ()
  (if (fboundp 'add-timeout)
      (itimer-timer-wakeup)
    (itimer-process-wakeup)))
