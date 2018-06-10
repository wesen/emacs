;;; python-components-test.el --- Part of python-components-mode


;; Maintainer: Andreas Roehler;; <andreas.roehler@online.de>
;; Keywords: languages, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Python-components-mode started from python-mode.el
;; and python.el. Tim Peters, Barry A. Warsaw, Skip
;; Montanaro, Ken Manheimer, Dave Love and many others
;; wrote major parts.

;;

;;; Code:

(defun python-mode-provide-test-buffer (&optional arg)
  (interactive "P")
  (when arg
    ;;      (eq 4 (prefix-numeric-value arg))
    (set-buffer (get-buffer-create "python-modes-test"))
    (erase-buffer)
    (insert "#! /usr/bin/env python
# -*- coding: utf-8 -*-\n
\"\"\" \"\"\"
import re, sys, os, pdb, random, time
import MySQLdb
from urllib2 import Request, urlopen, URLError, HTTPError
# Get the command line arguments
args = sys.argv
# pdb.set_trace()
# Get the name of the file to count the words in
filename = args[1]\n

def usage():
    print \"\"\"Usage: %s
    ....\"\"\" % (
        os.path.basename(sys.argv[0]))

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

def f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:

        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])

# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg

def openAnything(source):
    # try to open with urllib (if source is http, ftp, or file URL)
    import urllib
    try:
        return urllib.urlopen(source)
    except (IOError, OSError):
        pass

\"\"\"
This is \"a test\" string
on multiple lines.
\"\"\"
# https://bugs.launchpad.net/bugs/328807
def foo():
    x = 1
    # indentation is wrong after this comment
    y = 1

# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''

  \"\"\"
       \"Hi!\" I'm a doc string
    \"\"\"

# Source:
# http://launchpadlibrarian.net/22565844/test-triple-strings.py
# Author: Ed Loper

# Some easy cases:
\"O\" 'O' \"<'>\" '<\">'
 \"O\"   'O'   \"<'>\"   '<\">'
 \" O \"   ' O '   \" < ' > \"   ' < \" > '
\"\"\"O\"\"\" '''O''' \"<<<'>>>\" '''<\">'''

# Some harder cases:
\"\"\"<\">\"\"\" '''<'>'''

# Some tricky cases with backslashes.
'''<'>''' '''<\\'''>''' '''<\\\\'''

# Some tricky cases with more than 3 quotes in a row.
\"O\"\"\" \"O\"
\"\"\"\">\"\"\"
\"\"\"\">>\"\"\"
\"\"\"\"X\">\"\"\"
\"\"\"\"X\"\">\"\"\"
\"\"\"O\"\"\"\"O\" \"\"
\"\"\"O\"\"\"\"\" \"O\"
\"\"\"O\"\"\"\"\"\"<\">\"\"\"
\"\"\"O\"\"\"\"\"\"\"X\">\"\"\"
\"\"\"O\"\"\"\"\"\"\"\"X\">\"\"\"
\"\"\"O\"\"\" \"<<<>>>>\"
\"\"\"\"\"\"\"\"\"O\"\"\" \"O\"
\"\"\"O\"\"\"\"O\"\"O\"\"O\"\"
\"\"\"\">\"\"\"    \"\"\"\">>\"\"\"    \"\"\"\">>>\"\"\"
\"\"\"\"\">\"\"\"   \"\"\"\"\">>\"\"\"   \"\"\"\"\">>>\"\"\"
\"\"\"\"\">>>\"\"\"\"O\"   \"\"\"\"\">>>\"\"\"\"\"
\"\"\"\"\"\"\"\"\"<\"\"X\"X\"\">\"\"\"

# One version had a bug with comments ending in string markers: \"
\"\"\"O\"\"\"

\"\" \"\"

\"\"\"<\">\"\"\" '''<'>'''

# Spanning multiple lines:

\"<
>\"

'<
>'

\"\"\"
<
<
<
<
'
X
X
X
\"
>
>
\"\"\"

\"\"\"
Maintain the recent \"best\" price and alert when it remains the Maintain the recent \"best\" price and alert when it remains the
        best price for a user defined time.

\"\"\"

# indentation-with-backslash-line-continuation-629916
foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])

foo_long_long_long_long = \
    bar_long_long_long_long[
    (x_long_long_long_long == X) &
    (y_long_long_long_long == Y)]

")
    (set-buffer "python-modes-test")
    (goto-char 340)
    (when (featurep 'hs-minor-mode)(hs-minor-mode nil))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
;;    (python-mode)
    ))

;; (require 'python-components-mode)
(defun python-modes-indent-test (&optional arg)
  (interactive "p")
  (python-mode-provide-test-buffer arg)
  ;; will be replaced at release, loads
  ;; python-components-mode, also needed stuff from
  ;; S-X-Emacs-Werkstatt. Get the latter with `bzr
  ;; branch lp:s-x-emacs-werkstatt'
  (when (functionp 'python-mode-einrichtungen)(python-mode-einrichtungen))
  (when (featurep 'hs-minor-mode) (hs-minor-mode nil))
  (set-buffer "python-modes-test")
  (when (interactive-p) (switch-to-buffer (current-buffer)))
  (goto-char 1100))

(defalias 'pmt 'python-modes-test)
(defun python-modes-test (&optional arg)
  (interactive "p")
  (python-mode-provide-test-buffer arg)
  (when (featurep 'hs-minor-mode) (hs-minor-mode nil))
  (set-buffer "python-modes-test")
  (when (interactive-p) (switch-to-buffer (current-buffer)))
  (py-beginning-of-block-or-clause-test)
  (message "%s" "py-end-of-block-or-clause-test start")
  (sit-for 1)
  (py-end-of-block-or-clause-test)
  (sit-for 1)
  (py-beginning-of-class-test)
  (sit-for 1)
  (py-end-of-class-test)
  (sit-for 1)
  (py-beginning-of-clause-test)
  (sit-for 1)
  (py-end-of-clause-test)
  (sit-for 1)
  (py-beginning-of-def-or-class-test)
  (sit-for 1)
  (py-end-of-def-or-class-test)
  (sit-for 1)
  (py-beginning-of-statement-test)
  (sit-for 1)
  (py-end-of-statement-test))

;; Statement tests start
(defun py-beginning-of-block-or-clause-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 1400)
            (arg (or arg 1))
            (poss (list 67 30 80 75 60)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-beginning-of-block-or-clause)))))
          (when arg (sit-for arg)))
        (message "%s" "py-beginning-of-block-or-clause-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-end-of-block-or-clause-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 429)
            (arg (or arg 1))
            (poss (list -105 -67 -166 -290 -198)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-end-of-block-or-clause)))))
          (when arg (sit-for arg)))
        (message "%s" "py-end-of-block-or-clause-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-beginning-of-class-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 1400)
            (arg (or arg 1))
            (poss (list 1004)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-beginning-of-class)))))
          (when arg (sit-for arg)))
        (message "%s" "py-beginning-of-class-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-end-of-class-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 429)
            (arg (or arg 1))
            (poss (list -198)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-end-of-class)))))
          (when arg (sit-for arg)))
        (message "%s" "py-end-of-class-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-beginning-of-clause-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 1400)
            (arg (or arg 1))
            (poss (list 67 30 80 75 60)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-beginning-of-clause)))))
          (when arg (sit-for arg)))
        (message "%s" "py-beginning-of-clause-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-end-of-clause-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 429)
            (arg (or arg 1))
            (poss (list -105 -67 -166 -290 -198)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-end-of-clause)))))
          (when arg (sit-for arg)))
        (message "%s" "py-end-of-clause-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-beginning-of-def-or-class-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 1400)
            (arg (or arg 1))
            (poss (list 92 145 88 334 437)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-beginning-of-def-or-class)))))
          (when arg (sit-for arg)))
        (message "%s" "py-beginning-of-def-or-class-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-end-of-def-or-class-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 429)
            (arg (or arg 1))
            (poss (list -118 -256 -481 -290 -198)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-end-of-def-or-class)))))
          (when arg (sit-for arg)))
        (message "%s" "py-end-of-def-or-class-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-beginning-of-statement-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 1400)
            (arg (or arg 1))
            (poss (list 30 26 19 45 15)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-beginning-of-statement)))))
          (when arg (sit-for arg)))
        (message "%s" "py-beginning-of-statement-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

(defun py-end-of-statement-test (&optional arg)
  "With optional arg `python-mode-provide-test-buffer' is called first. "
  (interactive "p")
  (when arg (python-mode-provide-test-buffer t))
  (if (string= "python-modes-test" (buffer-name (current-buffer)))
      (let ((erg 429)
            (arg (or arg 1))
            (poss (list -46 -10 -31 -30 -137)))
        (set-buffer "python-modes-test")
        (when (interactive-p) (switch-to-buffer (current-buffer)))
        (setq poss (nreverse poss))
        (goto-char erg)
        (dolist (elt poss)
          (assert (eq elt (- erg (setq erg (py-end-of-statement)))))
          (when arg (sit-for arg)))
        (message "%s" "py-end-of-statement-test passed!"))
    (message "%s" "python-modes-test buffer needed!")))

;; Statement tests end

(defun py-leave-comment-or-string-backward-test ()
  (interactive "*")
  (goto-char 318)
  (end-of-line)
  (insert "\"")
  (assert (eq 327 (py-leave-comment-or-string-backward)))
  (py-leave-comment-or-string-backward)
  (message "%s" "py-leave-comment-or-string-backward-test done!"))

;; Independent tests
(defun tqs-302834-test ()
  "Run test for https://bugs.launchpad.net/python-mode/+bug/302834"
  (interactive)
  (independent-test-buffer-prepare 302834)
  (goto-char 78)
  (insert "\"")
  (assert (ar-in-string-p)))

(defalias 'py-goto-initial-line 'py-beginning-of-block)
(defun tqs-637955-test ()
  "Run test for https://bugs.launchpad.net/python-mode/+bug/637955"
  (interactive)
  (independent-test-buffer-prepare 637955)
  (goto-char (point-max))
  (py-beginning-of-def-or-class)
  (forward-line -1)
  (skip-chars-backward " \t\r\n\f")
  (py-goto-initial-line))

(defun independent-test-buffer-prepare (lp-bug-number)
  "Prepare a buffer for specific bug-fix checks before commit. "
  (set-buffer (get-buffer-create (concat "tqs-" (number-to-string lp-bug-number) "-test-buffer")))
  (erase-buffer)
  (insert "class OrderedDict(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
")
  (switch-to-buffer (current-buffer))
  (python-mode))

;; hack for FSF Emacs
(unless (fboundp 'read-shell-command)
  (defalias 'read-shell-command 'read-string))

(defun py-pychecker-run (command)
  "*Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (format "%s %s %s" py-pychecker-command
                   (mapconcat 'identity py-pychecker-command-args " ")
                   (buffer-file-name)))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(defun py-test-compute-indent ()
  (interactive)
  (let ((arg 6))
    (python-mode-provide-test-buffer)
    (python-mode-einrichtungen)
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (goto-char 1763)
    (py-beginning-of-statement)
    (dotimes (i arg)
      (assert (eq (progn (py-beginning-of-statement) (current-column)) (py-compute-indentation)))))
  (message "%s" "py-test-compute-indent passed!"))


(defun class-Rectangle-pep8-test (&optional arg load-branch-function)
  "An example from PEP 8, Style Guide for Python Code,
 Version: 88433"
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (class-Rectangle-pep8-intern arg))

(defun class-Rectangle-pep8-intern (&optional arg)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (lexical-let ((class-Rectangle-pep8-teststring "    class Rectangle(Blob):

        def __init__(self, width, height,
                     color='black', emphasis=None, highlight=0):
            if (width == 0 and height == 0 and
                color == 'red' and emphasis == 'strong' or
                highlight > 100):
                raise ValueError(\"sorry, you lose\")
            if width == 0 and height == 0 and (color == 'red' or
                                               emphasis is None):
                raise ValueError(\"I don't think so -- values are %s, %s\" %
                                 (width, height))
            Blob.__init__(self, width, height,
                          color, emphasis, highlight)
"))
    (if arg
        (progn
          (set-buffer (get-buffer-create "class-Rectangle-pep8"))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert class-Rectangle-pep8-teststring)
          (fundamental-mode)
          (class-Rectangle-pep8-base))
      (with-temp-buffer
        (insert class-Rectangle-pep8-teststring)
        (class-Rectangle-pep8-base)))))

(defun class-Rectangle-pep8-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 5)
  (fixup-whitespace)
  (indent-according-to-mode)
  (assert (eq 16 (current-indentation)) nil "class-Rectangle-pep8-test failed!")
  (forward-line 4)
  (fixup-whitespace)
  (indent-according-to-mode)
  (assert (eq 47 (current-indentation)) nil "class-Rectangle-pep8-test failed!")
  (message "%s" "class-Rectangle-pep8-test passed"))

(defun dict-with-continuation-lines-test (&optional arg load-branch-function)
  "Example from Mark Pilgrim's \"Dive into Python\".

Source: http://diveintopython.org "
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (dict-with-continuation-lines-test-intern arg))

(defun dict-with-continuation-lines-test-intern (&optional arg)
  "If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (lexical-let ((dict-with-continuation-lines-test-teststring "if __name__ == \"__main__\":
    myParams = {\"server\":\"mpilgrim\", \\
                \"database\":\"master\", \\
                \"uid\":\"sa\", \\
                \"pwd\":\"secret\" \\
                }
"))
    (if arg
        (progn
          (set-buffer (get-buffer-create "dict-with-continuation-lines-test"))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert dict-with-continuation-lines-test-teststring)
          (fundamental-mode)
          (dict-with-continuation-lines-test-base))
      (with-temp-buffer
        (insert dict-with-continuation-lines-test-teststring)
        (dict-with-continuation-lines-test-base)))))

(defun dict-with-continuation-lines-test-base ()
  (python-mode)
  (goto-char (point-min))
  (forward-line 1)
  (assert (eq 4 (py-compute-indentation)) nil "dict-with-continuation-lines-test failed!")
  (forward-line 1)
  (assert (eq 16 (py-compute-indentation)) nil "dict-with-continuation-lines-test failed!")
  (forward-line 3)
  (assert (eq 16 (py-compute-indentation)) nil "dict-with-continuation-lines-test failed!")
  (message "%s" "dict-with-continuation-lines-test passed"))

(provide 'python-components-test)
;;; python-components-test.el ends here
