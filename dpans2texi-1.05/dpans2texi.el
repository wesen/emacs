;;; dpans2texi.el --- Convert the ANSI Common Lisp draft to Texinfo

;; Copyright (C) 2004,2005,2008 Jesper Harder

;; Author: Jesper Harder <harder@phys.au.dk>
;; Created: 1 Mar 2004
;; Version: 1.04
;; Location: <http://purl.org/harder/dpans.html>
;; Keywords: Lisp, documentation

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;

;;; Commentary:
;;
;; This program converts the TeX sources for the draft proposed ANSI
;; Common Lisp standard to Texinfo.  The TeX sources are available
;; from ftp://parcftp.xerox.com/pub/cl/dpANS3 and
;; ftp://parcftp.xerox.com/pub/cl/dpANS3R
;;
;; It's by no means a general TeX to Texinfo converter -- the
;; conversion is only doable because most of the draft is written with
;; a set of mostly semantic macros.  In the cases where the draft
;; reverts to basic TeX, we mostly bail out, see `dp-special-cases'.
;;
;; The simpler TeX macros are implemented as Texinfo macros in
;; `dp.texi' (`dpi.texi' and `dph.texi' contain macros that should be
;; done differently for Info and HTML).
;;
;; `makeinfo' isn't multibyte clean, so we have to map multibyte chars
;; to unused 8bit chars, and post-process the Info files with `dp-tr'.
;; This isn't necessary for HTML.
;; 
;; To use `C-h C-i' (`info-lookup-symbol') to look up the symbol at
;; point in the manual, add the following to your .emacs:
;;
;; (require 'info-look)
;; 
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;; 
;;; Code:

(require 'rx)
(require 'cl)

(defvar dp-preamble
  "\\input texinfo                  @c -*-texinfo-*-
@c %**start of header
@setfilename ansicl
@settitle ANSI Common Lisp
@paragraphindent 0
@exampleindent 0
@documentencoding utf-8
@defindex sy
@c %**end of header
@dircategory Programming
@direntry
* ANSI Common Lisp: (ansicl).    Draft ANSI Common Lisp standard (dpANS3R).
@end direntry
@include dp.texi
@node Top
@top ANSI Common Lisp

This is a Texinfo version@footnote{The converter is available
from @uref{http://purl.org/harder/dpans.html}} of the draft
ANSI Common Lisp standard.  Some font information has been lost
in the conversion, and errors may have been introduced.  Report
discrepancies with the hardcopy version to
@email{jesper.harder@@gmail.com, Jesper Harder}.

@menu
")

(defvar dp-postamble
  "@node Index
@unnumbered Index
@printindex cp
@node Symbol Index
@unnumbered Symbol Index
@printindex sy
@node List of Figures
@unnumbered List of Figures
@listoffloats Figure
@ifnotinfo
@node Table of Contents
@unnumbered
@contents
@end ifnotinfo
@bye")

(defvar dp-list-type nil)
(defvar dp-current-label nil)
(defvar dp-nodes nil)
(defvar dp-dictionary-p nil)
(defvar dp-chapter-list nil)
(defvar dp-current-chapter nil)
(defvar dp-current-chapter-no)
(defvar dp-current-section-name)
(defvar dp-fig-no 0)
(defvar dp-section-no 0)
(defvar dp-subsection-no 0)
(defvar dp-subsubsection-no 0)
(defvar dp-subsubsubsection-no 0)
(defvar dp-subsubsubsubsection-no 0)
(defvar dp-current-chapter-marker (make-marker))
(defvar dp-current-section-marker)
(defvar dp-work-buffer)

(defvar dp-syntax-table
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\" "." table)
    table))

(defsubst dp-pm ()
  "Go to point-min."
  (goto-char (point-min)))

(defun dp-defconvert (function &rest tags)
  "Declare FUNCTION as a converter for TAGS."
  (mapc (lambda (x) (put x 'convert function)) tags))

(defun dp-parse-macros (file)
  "Parse Texinfo macros in FILE."
  (let (name arg args)
    (with-temp-buffer
      (insert-file-contents file)
      (insert-file-contents "dpi.texi")
      (dp-pm)
      (while (re-search-forward "^@r*macro \\([^{]+\\) +{\\([^}]*\\)}" nil t)
	(setq name (match-string 1)
	      arg (match-string 2))
	(setq args (length (split-string arg)))
	(put (intern name) 'convert
	     (intern (format "dp-arg%s" args)))))))

(defun dp-trim-whitespace (str)
  "Remove leading and trailing whitespace characters from STR."
  (when (string-match "\\`\\s-+" str)
    (setq str (substring str (match-end 0))))
  (when (string-match "\\s-+\\'" str)
    (setq str (substring str 0 (match-beginning 0))))
  str)

(defun dp-remove-whitespace (str)
  "Remove multiple whitespace from STR."
  (replace-regexp-in-string (rx (+ (or "\n" " "))) " " str t t))

(defun dp-freshline ()
  (unless (bolp)
    (insert "\n")))

(defun dp-strip-comments ()
  "Strip TeX comments."
  (dp-pm)
  (while (search-forward "%" nil t)
    (if (= (point) (1+ (point-at-bol)))
	;; remove entire line if % starts the line
	(dp-delete-line)
      (delete-region (1- (point)) (point-at-eol))))
  (dp-pm)
  (while (re-search-forward "\n\n\n+" nil t)
    (replace-match "\n\n")))

(defun dp-non-code ()
  "Strip comments and leading whitespace in non-code sections."
  (let (bpoint)
    (dp-pm)
    (while (not (eobp))
      (setq bpoint (point))
      (save-restriction
	(re-search-forward "^\\\\code" nil 'move-to-lmit)
	(narrow-to-region bpoint (point))
	(save-excursion
	  (dp-strip-comments)
	  (dp-pm)
	  ;; strip leading whitespace
	  (while (re-search-forward "^[ \t]+" nil t)
	    (replace-match ""))
	  (dp-hack-$)))
      (search-forward "\\endcode" nil 'move-to-limit))))

(defun dp-hack-~ ()
  "Convert ~ to @tie{} in non-code sections."
  (let (bpoint)
    (dp-pm)
    (while (not (eobp))
      (setq bpoint (point))
      (save-restriction
	(re-search-forward "^@lisp" nil 'move-to-lmit)
	(narrow-to-region bpoint (point))
	(dp-pm)
	(while (search-forward "~" nil t)
	  (replace-match "@tie{}")))
      (search-forward "@end lisp" nil 'move-to-limit))))

(defun dp-delete-line (&optional n)
  (delete-region (point-at-bol)
		 (progn (forward-line (or n 1)) (point))))

(defun dp-strip-newline (str &optional inplace)
  "Replace newlines with space in STR.
If INPLACE is non-nil, do it destructively"
  (subst-char-in-string ?\n ?\  str inplace))

(defun dp-quote-comma (str)
  "Quote commas in STR."
  (replace-regexp-in-string "," "\\," str nil t))

(defun dp-arg3 (command)
  (replace-match (concat "@" command) t t nil)
  (save-excursion
    (insert
     (dp-strip-newline
      (concat
       "{" (dp-quote-comma (dp-get-arg-delete)) ", "
       (dp-quote-comma (dp-get-arg-delete)) ", "
       (dp-quote-comma (dp-get-arg-delete)) "}")
      t))))

(defun dp-arg2 (command)
  (replace-match (concat "@" command) t t nil)
  (save-excursion
    (insert
     (dp-strip-newline
      (concat
       "{" (dp-quote-comma (dp-get-arg-delete)) ", "
       (dp-quote-comma (dp-get-arg-delete)) "}")
      t))))

(dp-defconvert 'dp-arg1 'i 'b 'loopref)
(defun dp-arg1 (command)
  "Convert 1 arg commands."
  (let ((p (make-marker)))
    (replace-match (concat "@" command) t t nil)
    (save-excursion
      (search-backward "@")
      (when (char-equal (preceding-char) ?{)
	(forward-char -1)
	(when (char-equal (preceding-char) ?\ )
	  (set-marker p (scan-sexps (point) 1))
	  (delete-char 1)
	  (goto-char p)
	  (delete-backward-char 1))))))


(defun dp-f (command)
  "Convert f commands."
  (replace-match "")
  (save-excursion
    (let ((arg (replace-regexp-in-string
		"~" "@tild{}" (dp-get-arg-delete))))
      (insert (concat "@f{" arg "}")))))

(dp-defconvert 'dp-arg0 'dots)
(defun dp-arg0 (command)
  (replace-match (concat "@" command "{}") t t nil))



(dp-defconvert 'dp-delete-command
	       'issue 'endissue 'bye 'endsubsubsubsubsection)

(defun dp-delete-command (command)
  (dp-delete-line))

(dp-defconvert 'dp-delete-command-1 'Vskip)
(defun dp-delete-command-1 (command)
  (delete-region (point-at-bol) (point-at-eol)))

(put 'code 'convert 'dp-translate)
(put 'code 'trans "@lisp")
(put 'endcode 'convert 'dp-translate)
(put 'endcode 'trans "@end lisp\n")

(put 'noindent 'convert 'dp-translate)
(put 'noindent 'trans "@noindent\n")

(put 't 'convert 'dp-translate)
(put 't 'trans "@code{t}")

(defun dp-translate (command)
  (replace-match (get (intern command) 'trans) t t nil))

(defun dp-strip-curly (str)
  (let (p)
    (with-current-buffer dp-work-buffer
      (erase-buffer)
      (insert str)
      (dp-pm)
      (skip-chars-forward " \n")
      (delete-region (point-min) (point))
      (when (looking-at "{")
	(setq p (scan-sexps (point) 1))
	(delete-char 1)
	(goto-char p)
	(delete-backward-char 1))
      (buffer-string))))

(defun dp-get-arg ()
  (dp-strip-curly (buffer-substring (point) (scan-sexps (point) 1))))

(defun dp-get-arg-delete ()
  (dp-strip-curly (delete-and-extract-region (point)
					     (scan-sexps (point) 1))))

;;; {\bf foo}  \bar{\bf foo}

(defun dp-brace-command (command)
  (let (newbrace)
    (replace-match "")
    (when (char-equal (following-char) ?\ )
      (delete-char 1))
    (assert (char-equal (char-before) ?{))
    (search-backward "{")
    (save-excursion
      (backward-char)
      (setq newbrace (looking-at "\\w")))
    (if newbrace
	(progn
	  (save-excursion
	    (forward-sexp)
	    (insert "}"))
	  (forward-char)
	  (insert "@" command "{"))
      (insert "@" command))))

;;; tables

(defvar dp-current-fig-label nil)
(dp-defconvert 'dp-DefineFigure 'DefineFigure)
(defun dp-DefineFigure (command)
  (replace-match "")
  (setq dp-current-fig-label (dp-get-arg-delete)))

(defun dp-get-rows (n)
  (let ((str (dp-get-arg-delete))
	(continue t)
	row rows p)
    (with-current-buffer dp-work-buffer
      (erase-buffer)
      (insert str)
      (dp-pm)
      (while continue
	(save-restriction
	  (narrow-to-region (point) (progn (search-forward "\\cr")
					   (point)))
	  (dp-pm)
	  (while (progn (setq p (point)) (search-forward "&" nil t))
	    (push (dp-trim-whitespace
		   (buffer-substring p (1- (point)))) row))
	  (setq p (point))
	  (search-forward "\\cr")
	  (push (dp-trim-whitespace
		 (buffer-substring p (- (point) 3))) row)
	  (dotimes (i (- n (length row)))
	    (push "" row))
	  (push (nreverse row) rows)
	  (setq row nil))
	(setq continue (save-excursion (search-forward "&" nil t)))))
    (nreverse rows)))

(defun dp-transpose (list)
  (apply 'mapcar* (cons 'list list)))

(defun dp-max-elems (list)
  (let ((max "") max-list)
    (dolist (row (dp-transpose list))
      (dolist (element row)
	(when (> (length element) (length max))
	  (setq max element)))
      (push max max-list)
      (setq max ""))
    (nreverse max-list)))

(put 'tablefigtwo 'num 2)
(put 'displaytwo 'num 2)
(put 'showtwo 'num 2)
(put 'tablefigthree 'num 3)
(put 'displaythree 'num 3)
(put 'showthree 'num 3)
(put 'tablefigfour 'num 4)
(put 'displayfour 'num 4)
(put 'displayfive 'num 5)
(put 'showfive 'num 5)
(put 'tablefigsix 'num 6)

(dp-defconvert 'dp-table
	       'tablefigtwo 'displaytwo 'showtwo
	       'tablefigthree 'displaythree 'showthree
	       'tablefigfour 'displayfour
	       'displayfive 'showfive 'tablefigsix)

(defvar dp-table-alist
  '(("CharSyntaxTypesInStdSyntax" . (9 26 6 22))
    ("fig2.10" . (29 9 6 11 10))
    ("fig4.9" . (7 7 52))
    ("fig5.5" . (45 28))
    ("fig5.6" . (45 28))
    ("fig24.1" . (35 29))
     ))

(defun dp-table (command)
  (replace-match "")
  (let ((n (get (intern command) 'num))
	(caption (dp-get-arg-delete))
	rows heads)
    (when (string-match "tablefig" command)
      (dotimes (i n)
	(push (dp-get-arg-delete) heads))
      (setq heads (nreverse heads)))
    (setq rows (dp-get-rows n))
    (when (null dp-current-fig-label)
      (setq dp-current-fig-label
	    (format "fig%s.%d" dp-current-chapter-no (1+ dp-fig-no))))
    (incf dp-fig-no)
    (save-excursion
      (insert "\n@float Figure," dp-current-fig-label)
      (insert "\n@cartouche\n")
      (insert "@multitable")
      (if (assoc dp-current-fig-label dp-table-alist)
	  (insert (mapconcat
		   (lambda (n)
		     (concat "{" (make-string n ?x) "}"))
		   (cdr (assoc dp-current-fig-label dp-table-alist)) ""))
	(dolist (elem (dp-max-elems (if heads (cons heads rows) rows)))
	  (insert (format "{%s}"
			  (with-temp-buffer
			    (set-syntax-table dp-syntax-table)
			    (insert (dp-strip-newline elem t))
			    (dp-pm)
			    (dp-convert)
			    (dp-pm)
			    (dp-hack-curly)
			    (buffer-string))))))
      (insert "\n")
      (when heads
	(insert "@headitem " (mapconcat 'identity heads " @tab ")))
      (dolist (row rows)
	(insert "\n@item " (mapconcat 'identity row " @tab ")))
      (insert "\n@end multitable\n")
      (insert "@end cartouche\n")
      (insert "@caption{" caption "}\n")
      (insert "@end float\n")))
  (setq dp-current-fig-label nil))

(dp-defconvert 'dp-quadrant 'dpquadrant)
(defun dp-quadrant (command)
  (incf dp-fig-no)
  (replace-match "@quadrant{}"))

(dp-defconvert 'dp-tabletwo 'tabletwo)
(defun dp-tabletwo (command)
  "Table in the Glossary."
  (let (head rows)
    (replace-match "")
    (insert "\n@multitable @columnfractions 0.25 0.7\n")
    (insert "\n@headitem " (dp-get-arg-delete) "@tab " (dp-get-arg-delete) "\n")
    (save-excursion
      (while (re-search-forward "\\entry{\\([^}]*\\)}{\\([^}]*\\)}" nil t)
	(push (list (match-string 1) (match-string 2)) rows)))
    (setq rows (nreverse rows))
    (dp-get-arg-delete)
    (save-excursion
      (dolist (row rows)
	(insert "\n@item " (mapconcat 'identity row " @tab ")))
      (insert "\n@end multitable\n"))))

(dp-defconvert 'dp-simplecaption 'simplecaption)
(defun dp-simplecaption (command)
  (replace-match "")
  (let ((arg (dp-get-arg-delete)))
    (save-excursion
      (search-backward "@lisp")
      (insert
       (format "@float Figure,fig%s.%d\n" dp-current-chapter-no
	       (incf dp-fig-no))))
    (insert "@caption{" arg "}\n@end float\n")))
	      
;;; Dictionary entries

(defvar dp-com-duplicates
  '("lambda" "function" "nil" "not" "t" "eql"
    "and" "or" "values" "error" "abort" "continue"
    "muffle-warning" "store-value" "use-value"
    "mod" "complex" "rational" "float"
    "cons" "atom" "list" "null" "member" "initialize-instance"
    "vector" "bit" "string" "pathname" "shared-initialize"
    "logical-pathname" "character" "-" "+" "*" "/" "1-" "time")

  "Duplicate node names aren't allowed in Texinfo.
We must prepend the type to these nodes.")

(defvar dp-anchors nil)

(dp-defconvert 'dp-begincom 'begincom)
(defun dp-begincom (command)
  "Beginning of a dictionary entry."
  (let (node-name dname type beg names)
    (setq beg (point-at-bol))
    (setq dname (dp-remove-whitespace (dp-get-arg)))
    (search-forward "\\ftype")
    (setq type (dp-get-arg))
    (delete-region beg (scan-sexps (point) 1))
    (setq node-name
	  (subst-char-in-string
	   ?, ?\;
	   (replace-regexp-in-string "[()]" "" dname) t))
    (when (> (length node-name) 72)
      (setq node-name (concat (substring node-name 0 71) "+")))
    (unless dp-dictionary-p
      (setq dp-dictionary-p t)
      (push 'Dictionary dp-nodes))
    (when (member node-name dp-com-duplicates)
      (setq node-name (concat node-name " (" type ")")))
    (push node-name dp-nodes)
    (insert "@node " node-name "\n")
    (insert "@heading " dname " (" type ")\n")
    (setq names (split-string dname ", *"))
    (dolist (name names)
      (insert "@syindex " name "\n")
      (insert "@cindex " name "\n"))
    (when (> (length names) 1)
      (dolist (name names)
	(insert "@anchor{" name "}\n")
	(push name dp-anchors)))))

(dp-defconvert 'dp-endcom 'endcom)
(defun dp-endcom (command)
  (setq dp-current-label nil)
  (dp-delete-line))

(dp-defconvert 'dp-label 'label)
(defun dp-label (command)
  (let (label)
    (skip-chars-forward " ")
    (setq label (buffer-substring
		 (point) (search-forward ":")))
    (if (looking-at "\\\\None.")
	(progn
	  (dp-delete-line)
	  (delete-region (point-at-bol)
			 (progn
			   (skip-chars-forward " \n\t")
			   (point))))
      (dp-delete-line)
      (setq dp-current-label label)
      (insert "@subsubheading " label "\n"))))

;; Sections, chapters

(defvar dp-chapter-name-alist nil)

(defvar dp-section-names nil)
(defun dp-get-section-names ()
  (setq dp-section-names nil)
  (setq dp-chapter-name-alist nil)
  (let (label name)
    (with-temp-buffer
      (insert-file-contents "setup-sections.tex")
      (dp-pm)
      (while (re-search-forward "\\\\def\\\\\\([^{]+\\)" nil t)
	(setq label (match-string 1))
	(setq name (subst-char-in-string ?: ?. (replace-regexp-in-string "[{}]" "" (dp-get-arg)) t))
	(push (cons label name) dp-section-names)
	(when (<= (length (split-string name "\\.")) 3)
	  (string-match "(\\([^)]+\\))" name)
	  (push (cons label (match-string 1 name)) dp-chapter-name-alist)))
      (erase-buffer)
      (insert-file-contents "setup-figures.tex")
      (dp-pm)
      (while (re-search-forward "\\\\def\\\\\\([^{]+\\)" nil t)
	(setq label (match-string 1))
	(setq name (subst-char-in-string
		    ?~ ?\  (replace-regexp-in-string "--" "." (dp-get-arg)) t))
	(push (cons label name) dp-section-names)))))

(dp-defconvert 'dp-beginchapter 'beginchapter)
(defun dp-beginchapter (command)
  (setq dp-current-chapter-no (dp-get-arg-delete))
  (let ((name (dp-get-arg-delete))
	(ref-name (dp-get-arg-delete)))
    (dp-delete-line)
    (insert "@node " name "\n")
    (if (string= dp-current-chapter-no "A")
	(insert "@appendix " name "\n")
      (insert "@chapter " name "\n"))
    (setq dp-current-chapter name)
    (setq dp-current-chapter-marker (point))
    (push name dp-chapter-list)
    (message "Converting %s" name)))

(dp-defconvert 'dp-endchapter 'endchapter)
(defun dp-endchapter (command)
  (save-excursion
    (dp-delete-line)
    (goto-char dp-current-chapter-marker)
    (when dp-nodes
      (insert "@menu\n")
      (setq dp-nodes (nreverse dp-nodes))
      (dolist (node dp-nodes)
	(if (symbolp node)
	    (insert (format "\n%s\n\n" node))
	  (insert "* " node "::\n")))
      (insert "@end menu\n"))
    (setq dp-current-chapter nil
	  dp-current-chapter-marker nil
	  dp-nodes nil
	  dp-dictionary-p nil
	  dp-fig-no 0
	  dp-section-no 0)))

(dp-defconvert 'dp-beginsection 'beginSection)
(defun dp-beginsection (command)
  (let* ((secname (dp-get-arg))
        (node-name  (subst-char-in-string ?, ?\; secname nil)))
    (setq dp-current-section-name node-name)
    (push node-name dp-nodes)
    (dp-delete-line)
    (insert "@node " node-name "\n"
           "@section " secname "\n")
    (incf dp-section-no)
    (setq dp-current-section-marker (point))))

(defvar dp-subsections-list nil)

(dp-defconvert 'dp-endsection 'endSection)
(defun dp-endsection (command)
  (dp-delete-line)
  (setq dp-subsection-no 0)
  (setq dp-subsubsection-no 0)
  (setq dp-subsubsubsection-no 0)
  (save-excursion
    (goto-char dp-current-section-marker)
    (when dp-subsections-list
      (when (search-forward "@node" nil t)
	(goto-char (point-at-bol))
	(insert "@menu\n")
	(setq dp-subsections-list (nreverse dp-subsections-list))
	(dolist (node dp-subsections-list)
	  (insert "* " node "::\n"))
	(insert "@end menu\n"))))
  (setq dp-subsections-list nil))

(dp-defconvert 'dp-beginsubsection
  'beginsubsection
  'beginSubsection
  'beginsubSection)

(defun dp-beginsubsection (command)
  (let ((name (dp-get-arg)))
    (setq name
	  (with-temp-buffer
	    (set-syntax-table dp-syntax-table)
	    (insert name)
	    (goto-char (point-min))
	    (dp-convert)
	    (buffer-string)))
    (setq dp-current-section-name name)
    (dp-delete-line)
    (insert "@node " name "\n")
    (push name dp-subsections-list)
    (insert "@subsection " name "\n")
    (incf dp-subsection-no)))

(dp-defconvert 'dp-endsubsection
	       'endSubsection 'endsubsection 'endSubsection
	       'endsubSection)

(defun dp-endsubsection (command)
  (dp-delete-line)
  (setq dp-subsubsection-no 0
	dp-subsubsubsection-no 0))

(dp-defconvert 'dp-beginsubsubsection 'beginsubsubsection)
(defun dp-beginsubsubsection (command)
  (let ((name (dp-get-arg)))
    (setq name
	  (with-temp-buffer
	    (set-syntax-table dp-syntax-table)
	    (insert name)
	    (goto-char (point-min))
	    (dp-convert)
	    (buffer-string)))
    (dp-delete-line)
    (insert "@subsubsection " name "\n")
    (incf dp-subsubsection-no)))

(dp-defconvert 'dp-endsubsubsection 'endsubsubsection)
(defun dp-endsubsubsection (command)
  (dp-delete-line)
  (setq dp-subsubsubsection-no 0))

(dp-defconvert 'dp-beginsubsubsubsection'beginsubsubsubsection)
(defun dp-beginsubsubsubsection (command)
  (replace-match "")
  (let ((name (dp-get-arg-delete)))
    (setq dp-current-section-name name)
    (insert
     (format "@unnumberedsubsubsec %s.%d.%d.%d.%d %s\n"
	     dp-current-chapter-no
	     dp-section-no dp-subsection-no
	     dp-subsubsection-no
	     (incf dp-subsubsubsection-no) name))))

(dp-defconvert 'dp-endsubsubsubsection 'endsubsubsubsection)
(defun dp-endsubsubsubsection (command)
  (dp-delete-line)
  (setq dp-subsubsubsubsection-no 0))

(dp-defconvert 'dp-beginsubsubsubsubsection 'beginsubsubsubsubsection)
(defun dp-beginsubsubsubsubsection (command)
  (replace-match "")
  (let ((name (dp-get-arg-delete)))
    (setq dp-current-section-name name)
    (insert
     (format "@unnumberedsubsubsec %s.%d.%d.%d.%d.%d %s\n"
	     dp-current-chapter-no
	     dp-section-no dp-subsection-no
	     dp-subsubsection-no
	     dp-subsubsubsection-no
	     (incf dp-subsubsubsubsection-no) name))))

(dp-defconvert 'dp-definesection 'DefineSection)
(defun dp-definesection (command)
  (replace-match "")
  (let ((name (dp-get-arg-delete)))
    (unless (string= name dp-current-section-name)
      (insert "@anchor{" name "}")
      (push name dp-anchors))))

(dp-defconvert 'dp-vskip 'vskip)
(defun dp-vskip (command)
  (replace-match "")
  (delete-region (point) (search-forward "pt")))

;; multi

(dp-defconvert 'dp-defun-multi-with-values 'DefunMultiWithValues)
(defun dp-defun-multi-with-values (command)
  (replace-match "")
  (let ((arg1 (dp-quote-comma (dp-get-arg-delete)))
	(arg2 (dp-quote-comma (dp-get-arg-delete)))
	(arg3 (dp-get-arg-delete))
	entries)
    (with-temp-buffer
      (insert arg3)
      (dp-pm)
      (while (search-forward "\\entry" nil t)
	(push (dp-get-arg) entries)))
    (setq entries (nreverse entries))
    (save-excursion
      (dolist (entry entries)
	(insert "@DefunWithValues{" entry ", " arg1 ", " arg2 "}\n")))))

(dp-defconvert 'dp-defun-multi-accessor-with-values 'DefunMultiAccessorWithValues)
(defun dp-defun-multi-accessor-with-values (command)
  (replace-match "")
  (let ((arg1 (dp-get-arg-delete))
	(arg2 (dp-get-arg-delete))
	(arg3 (dp-get-arg-delete))
	(arg4 (dp-get-arg-delete))
	entries)
    (with-temp-buffer
      (insert arg4)
      (dp-pm)
      (while (re-search-forward (rx (or "\\entry" "\\blankline")) nil t)
	(when (string= (match-string 0) "\\entry")
	  (push (dp-get-arg) entries))))
    (setq entries (nreverse entries))
    (save-excursion
      (dolist (entry entries)
	(insert entry " " arg1 " @EV{} " arg2 "  |  (setf (" arg1 " " arg2 ") " arg3 ")@*\n"))
      (insert "@*\n"))))

(dp-defconvert 'dp-defsetf-multi 'DefsetfMulti)
(defun dp-defsetf-multi (command)
  (replace-match "")
  (let ((arg1 (dp-get-arg-delete))
	(arg2 (dp-get-arg-delete))
	(arg3 (dp-get-arg-delete))
	entries)
    (with-temp-buffer
      (insert arg3)
      (dp-pm)
      (while (search-forward "\\entry" nil t)
	(push (dp-get-arg) entries))
      (setq entries (nreverse entries))
      (save-excursion
	(dolist (entry entries)
	  (insert "@defsetf{" entry ", " arg1 ", " arg2 "}@*\n"))
	(insert "@*\n")))))

(dp-defconvert 'dp-docmethods 'DocMethods)
(defun dp-docmethods (command)
  (replace-match "")
  (let ((arg1 (dp-get-arg-delete))
	(arg2 (dp-get-arg-delete))
	entries)
    (with-temp-buffer
      (insert arg2)
      (dp-pm)
      (while (search-forward "\\Meth" nil t)
	(push (list (dp-get-arg) (dp-get-arg)) entries)))
    (save-excursion
      (insert arg1 "\n\n")
      (dolist (entry entries)
	(insert "documentation (@var{x} @code{" (car entry)
		"}) (@var{doc-type} @f{(eql '" (cadr entry)
		")}) " (cadr entry) "\n\n" ))
      (dolist (entry entries)
	(insert "(setf documentation) @var{new-value} (@var{x} @code{"
		(car entry) "}) (@var{doc-type} @f{(eql '"
		(cadr entry) ")}) " (cadr entry) "\n\n")))))

(dp-defconvert 'dp-Defmeth 'Defmeth)
(defun dp-Defmeth (command)
  (replace-match ""))

;;; Glossary

(dp-defconvert 'dp-indextab 'indextab)
(defun dp-indextab (command)
  (let ((arg (dp-get-arg)))
    (push arg dp-nodes)
    (dp-delete-line)
    (insert "\n@end table\n"
	    "@node " arg "\n"
	    "@unnumberedsec " arg "\n"
	    "@table @asis\n")))

(dp-defconvert 'dp-firstindextab 'firstindextab)
(defun dp-firstindextab (command)
  (let ((arg (dp-get-arg)))
    (push arg dp-nodes)
    (dp-delete-line)
    (insert "@node " arg "\n"
	    "@unnumberedsec " arg "\n"
	    "@table @asis\n")))

(dp-defconvert 'dp-gentry 'gentry)
(defun dp-gentry (command)
  (replace-match "")
  (let ((arg (dp-get-arg-delete)))
    (insert "@item @b{" arg "} @anchor{glos-" arg "}\n")
    (push (concat "glos-" arg) dp-anchors)))

;;; Lists

(dp-defconvert 'dp-beginlist 'beginlist)
(defun dp-beginlist (command)
  (let (arg)
    (save-excursion
      (re-search-forward (rx (or "\\itemitem" "\\item")))
      (setq arg (dp-get-arg))
      (cond ((string= arg "1.")
	      (push 'enumerate dp-list-type)
	      (setq arg "1"))
	     ((string= arg "a.")
	      (push 'enumerate dp-list-type)
	      (setq arg "a"))
	     ((string= arg "--")
	      (push 'itemize dp-list-type))
	     ((string= arg "\\bull")
	      (push 'itemize dp-list-type)
	      (setq arg "@bullet{}"))
	     (t
	      (push 'table dp-list-type))))
    (dp-delete-line)
    (ecase (car dp-list-type)
      (enumerate (insert "\n@enumerate " arg "\n"))
      (itemize (insert "\n@itemize " arg "\n"))
      (table (insert "\n@table @asis\n")))))

(dp-defconvert 'dp-endlist 'endlist)
(defun dp-endlist (command)
  (dp-delete-line)
  (when (save-excursion  ;; i.e. `looking-back'
	  (re-search-backward "\\(?:\n\n\\)\\=" nil t))
    (delete-char -1))
  (ecase (pop dp-list-type)
    (enumerate (insert "@end enumerate\n\n"))
    (itemize (insert "@end itemize\n\n"))
    (table (insert "@end table\n\n"))))

(dp-defconvert 'dp-item 'itemitem 'item)
(defun dp-item (command)
  (let (arg)
    (ecase (car dp-list-type)
      ((enumerate itemize)
       (delete-region (point-at-bol) (scan-sexps (point) 1))
       (insert "@item"))
      (table
       (replace-match "@item" nil t)
       (setq arg (dp-strip-newline (dp-get-arg-delete)))
       (save-excursion
	 (insert " " "@id{" arg "}\n"))))))

;;; Cross references

(defvar dp-funref-name-alist
  '(("bit" . "bit")
    ("list" . "list")
    ("member" . "member")
    ("rational" ."rational")
    ("use-value" . "use-value")
    ("store-value" . "store-value")
    ("continue" . "continue")
    ("abort" . "abort")
    ("muffle-warning" ."muffle-warning")
    ("values" . "values (Accessor)")
    ("shared-initialize" . "shared-initialize (Standard Generic Function)")
    ("initialize-instance" . "initialize-instance (Standard Generic Function)")))

(dp-defconvert 'dp-varref
   'varref 'seevar 'Seevar
   'specref 'seespec 'Seespec
   'macref 'seemac 'Seemac
   'typeref 'seetype 'Seetype
   'conref 'declref
   'funref 'seefun 'Seefun
   'seefuns 'Seefuns 'function
   'misc 'seemisc 'Seemisc
   'packref
   'seeterm 'Seeterm 'SeetermAlso 'seetermAlso)

(defun dp-varref (command)
  (replace-match "")
  (setq command (intern command))
  (let* ((arg (dp-get-arg-delete))
	 (link arg))
    (when (member arg dp-com-duplicates)
      (setq
       link
       (ecase command
	 ((seefun Seefun seefuns Seefuns function funref)
	  (if (assoc arg dp-funref-name-alist)
	      (cdr (assoc arg dp-funref-name-alist))
	    (concat arg " (Function)")))
	 ((specref seespec Seespec)
	  (concat arg " (Special Operator)"))
	 ((seemac Seemac macref)
	  (concat arg " (Macro)"))
	 ((seetype Seetype typeref)
	  (concat arg " (System Class)"))
	 (conref
	  (concat arg " (Constant Variable)"))
	 (declref
	  (concat arg " (Type Specifier)"))
	 ((misc seemisc Seemisc)
	  (concat arg " (Symbol)"))
	 ((Seeterm seeterm SeetermAlso seetermAlso)
	  arg)
	 )))
    (if (memq command '(seeterm Seeterm SeetermAlso seetermAlso))
	(setq link (concat "@ref{glos-" arg ", " arg "}"))
      (setq link (concat "@ref{" link "}")))
    (ecase command
      (seefun (insert "see the @term{function} " link))
      (Seefun (insert "See the @term{function} " link))
      (seefuns (insert "see the @term{functions} " link))
      (Seefuns (insert "See the @term{functions} " link))
      (seevar (insert "see the @term{variable} " link))
      (Seevar (insert "See the @term{variable} " link))
      (seespec (insert "see the @term{special operator} " link))
      (Seespec (insert "See the @term{special operator} " link))
      (seemac (insert "see the @term{macro} " link))
      (Seemac (insert "See the @term{macro} " link))
      (seetype (insert "see the @term{type} " link))
      (Seetype (insert "See the @term{type} " link))
      (seemisc (insert "see " link))
      (Seemisc (insert "See " link))
      (Seeterm (insert "See " link))
      (seeterm (insert "see " link))
      (SeetermAlso (insert "See also " link))
      (seetermAlso (insert "see also " link))
      ((varref specref macref typeref conref declref funref function)
       (if (string= dp-current-label "See Also:")
	   (insert link)
	 (insert "@code{" arg "}")))
      (misc
       (if (string= dp-current-label "See Also:")
	   (insert link)
	 (insert "@t{" arg "}")))
      (packref
       (setq arg (upcase arg))
       (if (string= dp-current-label "See Also:")
	   (insert "@ref{" arg "}")
	 (insert "@code{" arg "}")))
      )))

(dp-defconvert 'dp-seeref
	       'seechapter 'Seechapter
	       'seefigure 'Seefigure
	       'seesection 'Seesection
	       'secref 'chapref
	       'figref 'Figref)

(defun dp-seeref (command)
  (let (arg text)
    (save-match-data
      (re-search-forward "\\\\\\(\\w+\\)")
      (setq arg (match-string 1))
      (replace-match ""))
    (setq text (cdr (assoc arg dp-section-names)))
    (assert text)
    (when (assoc arg dp-chapter-name-alist)
      (setq arg (cdr (assoc arg dp-chapter-name-alist))))
    (ecase (intern command)
      ((seechapter seefigure seesection)
       (replace-match (concat "see @ref{" arg ", " text "}") t t nil))
      ((Seechapter Seefigure Seesection)
       (replace-match (concat "See @ref{" arg ", " text "}") t t nil))
      ((figref Figref chapref secref)
       (replace-match (concat "@ref{" arg ", " text "}") t t nil)))
    (when (looking-at "}")
      (save-excursion
	(goto-char
	 (prog1
	     (scan-sexps (1+ (point)) -1)
	   (replace-match "")))
	(delete-char 1)))))

(dp-defconvert 'dp-thepackage 'thepackage 'Thepackage)
(defun dp-thepackage (command)
  (replace-match "")
  (let ((arg (dp-get-arg-delete)))
    (if (eq (aref command 0) ?T)
	(insert "T")
      (insert "t"))
    (insert "he @code{" (upcase arg) "} @term{package}")))

;;; Index entries

(dp-defconvert 'dp-idx
  'idxref 'idxterm 'idxtext 'idxexample
  'idxpackref 'idxcode 'idxkwd 'idxkeyref)

(defun dp-idx (command)
  (replace-match "")
  (let ((arg (dp-get-arg-delete)))
    (dp-freshline)
    (insert
     "@cindex "
     (case (intern command)
       (idxkeyref (concat "&" arg))
       (idxpackref (upcase arg))
       (idxkwd (concat ":" arg))
       (t arg)))
    (unless (eolp)
      (skip-chars-forward " ")
      (insert "\n"))))

(dp-defconvert 'dp-newtermidx 'newtermidx)
(defun dp-newtermidx (command)
  (replace-match "")
  (let ((arg1 (dp-get-arg-delete))
	(arg2 (dp-get-arg-delete)))
    (dp-freshline)
    (insert "@cindex " arg2 "\n")
    (insert "@dfn{" arg1 "}")))

;;; $

(defun dp-hack-$ ()
  "Convert math environments."
  (dp-pm)
  (while (search-forward "$$" nil t)
    (replace-match "\n@quotation\n\\\\mat{")
    (search-forward "$$")
    (replace-match "}\n@end quotation\n"))
  (dp-pm)
  (while (re-search-forward
	  (rx (and (+ "$") (group (* (not (in "$")))) (+ "$"))) nil t)
    (replace-match "\\\\mat{\\1}" t)))

;;; Subscripts

(defvar dp-sub-alist
  '((?0 . "@sub0{}")
    (?1 . "@sub1{}")
    (?2 . "@sub2{}")
    (?3 . "@sub3{}")
    (?4 . "@sub4{}")
    (?5 . "@sub5{}")
    (?6 . "@sub6{}")
    (?7 . "@sub7{}")
    (?8 . "@sub8{}")
    (?9 . "@sub9{}")))

(dp-defconvert 'dp-sub 'sub)
(defun dp-sub (command)
  (let (arg)
    (replace-match "")
    (delete-region (point) (progn (skip-chars-forward " \n") (point)))
    (if (char-equal (following-char) ?{)
	(setq arg (dp-get-arg-delete))
      (setq arg
	    (delete-and-extract-region
	     (point)
	     (progn
	       (skip-chars-forward "a-zA-Z0-9 ")
	       (point)))))
    (setq arg (dp-trim-whitespace arg))
    (if (string-match "^[0-9]+$" arg)
	(insert (mapconcat (lambda (char)
			     (cdr (assoc char dp-sub-alist)))
			   arg ""))
      (if (= (length arg) 1)
	  (insert "@subs1{" arg "}")
	(save-excursion
	  (insert "@subs{" arg "}"))))))

(dp-defconvert 'dp-meaning 'meaning)
(defun dp-meaning (command)
  (replace-match "")
  (let ((arg (dp-get-arg-delete)))
    (if (string-match "^[0-9]+$" arg)
	(insert (mapconcat (lambda (char)
			     (cdr (assoc char dp-sub-alist)))
			   arg ""))
      (insert "[" arg "]"))))

;;; Credits

(defun dp-hack-credits ()
  "Do the Credits chapter."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\\goodbreak" nil nil 2)
    (delete-region (point-min) (point))
    (push "Credits" dp-chapter-list)
    (insert "@node Credits\n"
	    "@unnumbered Credits\n"
	    "@editors\n")
    (dp-hack-credits-1 3 3 "Edit and Review History:" "0.15 0.15 0.7")
    (dp-hack-credits-1 2 3 "Ad Hoc Group Chairs:" "0.5 0.5")
    (dp-hack-credits-1 2 4 "Major Administrative Contributions:" "0.5 0.5")
    (dp-hack-credits-1 3 3 "Major Technical Contributions:" "0.33 0.33 0.33")
    (dp-hack-credits-1 2 3 "Participating Companies and Organizations:" "0.5 0.5")
    (dp-hack-credits-1 3 3 "Individual Participants:" "0.33 0.33 0.33")))

(defun dp-hack-credits-1 (columns delete heading fractions)
  (search-forward "$$")
  (dp-delete-line delete)
  (insert "@subheading " heading "\n")
  (insert "@multitable @columnfractions " fractions "\n")
  (save-restriction
    (narrow-to-region (point) (search-forward "$$"))
    (goto-char (point-min))
    (insert "{")
    (goto-char (point-max))
    (insert "}")
    (goto-char (point-min))
    (dolist (row (dp-get-rows columns))
      (insert "\n@item " (mapconcat 'identity row " @tab "))))
  (re-search-forward "\\$\\$}")
  (replace-match "\n@end multitable"))

;;; 

(defun dp-insert ()
  "Handle include directives."
  (let (file)
    (dp-pm)
    (while (re-search-forward "^[^%]*\\\\\\input \\([^ \t%\n]+\\)" nil t)
      (setq file (concat (match-string 1) ".tex"))
      (dp-delete-line)
      (unless (string= file "setup.tex")
	(save-restriction
	  (narrow-to-region (point) (point))
	  (insert-file-contents-literally file))))
    (dp-pm)
    (while (search-forward "\\includeDictionary" nil t)
      (setq file (concat (dp-get-arg-delete) ".tex"))
      (dp-delete-line)
      (save-restriction
	(narrow-to-region (point) (point))
	(insert-file-contents-literally file)))))

(defun dp-hack-curly ()
  "This is just a band-aid for not parsing TeX curlies well enough.
Yuck."
  (let (multi (p (make-marker)))
    (while (search-forward "{" nil t)
      (save-excursion
	(forward-char -1)
	(skip-chars-backward "a-zA-Z0-9")
	(if (looking-at "multitable")
	    (setq multi t)
	  (unless (char-equal (preceding-char) ?@)
	    (search-forward "{")
	    (forward-char -1)
	    (set-marker p (scan-sexps (point) 1))
	    (delete-char 1)
	    (goto-char p)
	    (delete-backward-char 1))))
      (when multi
	(forward-line)
	(setq multi nil)))))

(defun dp-quote-special ()
  "Deal with character that special in either Tex or Texinfo."
  (let ((alist
	 '(("@" . "@@") ("\\ " . "@spc{}")
	   ("\\," . "") ("\\\\" . "@bsl{}")
	   ("\\_" . "_") ("\\&" . "@ampers{}")
	   ("\\#" . "#") ("\\$" . "@dollar{}")
	   ("\\%" . "@percent{}") ("\\{" . "@lcurly{}")
	   ("\\}" . "@rcurly{}") ("\\/" . "")
	   ("$-$" . "-") ("$+$" . "+")
	   ("\\\n" . "\n") ("\t" . ""))))
    (dolist (elem alist)
      (dp-pm)
      (while (search-forward (car elem) nil t)
	(replace-match (cdr elem) t t nil)))))

;;; Annoying cases

(defun dp-substitute (table)
  (dolist (row table)
    (dp-pm)
    (while (re-search-forward (concat "\\(\\\\" (car row) "\\)\\W") nil t)
      (replace-match (cdr row) t t nil 1))))

(defvar dp-charsyntaxtypes-table
  '(("w" . "@term{whitespace}@sub2{}")
    ("n" . "@term{non-terminating} @term{macro char}")
    ("t" . "@term{terminating} @term{macro char}")
    ("c" . "@term{constituent}")
    ("C" . "@term{constituent}*")
    ("SE" . "@term{single escape}")
    ("ME" . "@term{multiple escape}")))

(defvar dp-sharpsign-table
  '(("u" . "undefined")
    ("s" . "signals error")
    ("ia" . "infix argument")))

(defvar dp-constituent-table
  '(("a" . "@term{alphabetic}@sub2{}")
    ("ad" . "alphadigit")
    ("i" . "@term{invalid}")
    ("pm" . "@term{package marker}")))

(defun dp-search-delete (str n)
  "Search for STR from bob and delete N lines."
  (dp-pm)
  (when (search-forward str nil t)
    (dp-delete-line n) t))

(defun dp-special-cases ()
  "Handle special cases.
This would probably be better done with a diff.  Oh, well."
  (dp-pm)
  (search-forward "$$ \\ff{arctanh} z = {{\\ff{log} (1+z) - \\ff{log} (1-z)}\\over{2}}. $$ ")
  (dp-delete-line)
  (insert "$$ \\ff{arctanh} z = (\\ff{log} (1+z) - \\ff{log} (1-z))/2. $$ ")
  (dp-pm)
  (search-forward "$$ \\ff{arctan} z = {{\\ff{log} (1+iz) - \\ff{log} (1-iz)}\\over{2i}} $$")
  (dp-delete-line)
  (insert "$$ \\ff{arctan} z = (\\ff{log} (1+iz) - \\ff{log} (1-iz))/2i $$")
  (dp-pm)
  (while (re-search-forward "}\\( *-- *\\)[^-]" nil t)
    (replace-match "---" t t nil 1))
  (dp-pm)
  (while (search-forward "\\\"u" nil t)
    (replace-match "\\uumlaut{}" t t))
  (dp-pm)
  (while (dp-search-delete "\\noalign{\\vskip " 1))
  (dp-pm)
  (search-forward "Defun floatp")
  (replace-match "DefunWithValues floatp")
  (dp-pm)
  (search-forward "Defun find-restart")
  (replace-match "DefunWithValues find-restart")
  (dp-pm)
  (search-forward "constructor-function-name" nil t) ;; dict-structures
  (forward-line -2)
  (dp-delete-line 8)
  (insert
   "\\code
 (constructor-function-name
  slot-keyword1 form-1
  slot-keyword2 form-2
  ...)
\\endcode")
  (dp-pm)
  (while (search-forward "\\item{{" nil t)
    (forward-char -2)
    (save-excursion
      (forward-sexp)
      (delete-char -1))
    (delete-char 1))
  (dp-pm)
  (search-forward "{\\def\\Qfont" nil t)
  (dp-delete-line 16)
  (insert "\\dpquadrant\n")
  ;; dict-numbers
  (dp-search-delete "\\def\\realtypespec" 20)
  ;; dict-numbers
  (dp-search-delete "\\def\\Result{" 3)
  (dp-pm) ;; dict-numbers
  (search-forward "\\def\\zz" nil t)
  (forward-line -1)
  (dp-delete-line 6)
  (search-forward "}\n}" nil t)
  (dp-delete-line)
  (dp-search-delete "\\def\\alfa" 1)
  ;; dict-conses
  (dp-search-delete "\\def\\SatisfyTest" 3)
  (dp-pm) ;; dict-arrays
  (search-forward "\\tabskip 2\\dimen" nil t)
  (forward-line -2)
  (dp-delete-line 7)
  (dp-delete-line 5) ;XXX: there are 5 lines in the source file,
                     ; but the \noalign lines are deleted above.
  (insert "\\tablefigtwo{Bit-wise Logical Operations on Bit Arrays}
{Function}{Operation}{")
  (search-forward "\\caption{Bit-wise")
  (dp-delete-line 3)
  (insert "}")
  (dp-pm)
  (search-forward "\\beginsubsubsubsection{Open and Closed Streams}" nil t)
  (forward-line)
  (dp-delete-line)
  ;; concept-files
  (dp-search-delete "\\DefineSection{Truenames}" 1)
  ;; dict-streams
  (dp-search-delete "\\def\\ExplainRecursiveP" 3)
  ; concept-format
  (dp-search-delete "\\def\\Partial" 1)
  ;; dict-printer
  (dp-search-delete "\\def\\writekeys" 35)
  (dp-pm)
  (while (search-forward "#\\b" nil t)  ;; bug in dict-printer.tex
    (replace-match "#@backslash{}b" t t nil))
  ;; concept-systems
  (dp-search-delete "\\DefineSection{Features" 1)
  (dp-pm) ;; concept-systems
  (search-forward "#+spice" nil t)
  (save-restriction
    (narrow-to-region (point) (progn (forward-line 28) (point)))
    (dp-pm)
    (while (search-forward "\\span" nil t)
      (replace-match "&"))
    (dp-pm))
  ;; dict-environment
  (dp-search-delete "\\def\\DocMethods" 6)
  ;; concetp-environment
  (dp-search-delete "\\DefineSection{Time" 1)
  (dp-pm)
  ;; dict-objects
  (dp-search-delete "$$\\vbox{\\halign{\\strut" 8)
  (insert "@initargs{}\n")
  (dp-search-delete "\\def\\GFauxOptionsAndMethDesc" 12) ;; dict-objects
  (dp-pm)
  (while (search-forward "\\!" nil t)
    (replace-match ""))
  (dp-pm) ;; dict-objects
  (search-forward "A \\macref{with-accessors} expression" nil t)
  (forward-line 2)
  (dp-delete-line 19)
  (insert "\\withaccessors\n")
  (search-forward "A \\macref{with-slots} expression" nil t)
  (forward-line 2)
  (dp-delete-line 31)
  (insert "\\withslots\n")
  (search-forward "\\begincom{defclass" nil t)
  (forward-line 11)
  (dp-delete-line 24)
  (insert "\\defclass\n")
  (dp-pm)
  (search-forward "\\begincom{defmethod" nil t)
  (forward-line 16)
  (dp-delete-line 26)
  (insert "\\defmethod\n")
  (dp-pm) ;; concept-loop
  (search-forward "\\kern-7pt" nil t)
  (replace-match "")
  (dp-search-delete "\\def\\subOne" 2) ;; dict-flow
  (dp-pm) ;; concept-bvl
  (search-forward "{\\def\\TVar{\\curly" nil t)
  (forward-line -1)
  (dp-delete-line 9)
  (insert "\\macrolambdalist\n")
  (search-forward "\\Vskip 1pc!")
  (forward-line -1)
  (dp-delete-line 2)
  (dp-pm)
  (while (search-forward "\\vfill" nil t)
    (dp-delete-line))
  (dp-pm)
  (search-forward "\\beginSection{Introduction}" nil t)
  (replace-match "\\beginSection{Introduction to Types and Classes}" t t)
  ;; concept-compile
  (dp-pm)
  (search-forward "\\offinterlineskip" nil t)
  (forward-line -3)
  (dp-delete-line 9)
  (insert "\\tablefigsix{EVAL-WHEN processing}{\\b{CT}}{\\b{LT}}{\\b{E}}{\\b{Mode}}{\\b{Action}}{\\b{New Mode}}{")
  (search-forward "\\endfig")
  (forward-line)
  (dp-delete-line -3)
  (insert "}")
  (dp-search-delete "\\def\\sim#1#2#3" 1)
  (dp-pm)
  (search-forward "{\\def\\TVar{\\curly" nil t)
  (forward-line -1)
  (dp-delete-line 9)
  (insert "\\dmacrolambdalist\n")
  (search-forward "\\Vskip")
  (forward-line -1)
  (dp-delete-line 2)
  (dp-pm)
  (search-forward "\\DefineFigure{CharSyntaxTypesInStdSyntax}" nil t)
  (forward-line 1)
  (dp-delete-line 8)
  (save-restriction
    (narrow-to-region
     (point)
     (progn
       (re-search-forward "^}}")
       (delete-char -1)
       (point)))
    (dp-pm)
    (dp-substitute dp-charsyntaxtypes-table))
  (dp-pm)
  (search-forward "{\\def\\u{undefined}" nil t)
  (dp-delete-line 3)
  (save-restriction
    (narrow-to-region
     (point)
     (progn
       (re-search-forward "^}}")
       (delete-char -1)
       (point)))
    (dp-pm)
    (dp-substitute dp-sharpsign-table))
  (dp-pm)
  (search-forward "\\DefineFigure{ConstituentTraitsOfStdChars}" nil t)
  (forward-line 1)
  (dp-delete-line 11)
  (insert "\\displayfour{Constituent Traits of Standard Characters and Semi-Standard Characters}{\n")
  (insert "\\b{constituent}&\\b{traits}&\\b{constituent}&\\b{traits}\\cr\n")
  (insert "\\b{characters}&&\\b{characters}\\cr\n")
  (save-restriction
    (narrow-to-region (point)
		      (progn
			(search-forward "\\endfig")
			(forward-line -2)
			(dp-delete-line 3)
			(insert "}")
			(point)))
    (dp-pm)
    (dp-substitute dp-constituent-table))
  (dp-pm)
  (search-forward "\\DefineFigure{SyntaxForNumericTokens}" nil t)
  (forward-line 1)
  (dp-delete-line 6)
  (insert "\\showthree{Syntax for Numeric Tokens}{")
  (search-forward "\\param{sign}---")
  (goto-char (point-at-bol))
  (insert "}\n")
  (search-forward "\\endfig")
  (forward-line -2)
  (dp-delete-line 3)
  (dp-pm)
  (search-forward "\$\\vert\\;\$" nil t)
  (replace-match "|")
  (dp-pm)
  (search-forward "\\beginSection{Glossary}" nil t)
  (dp-delete-line 1)
  (insert "\\beginSection{Glossary Notation}\n")
  (search-forward "\\def\\gentry" nil t)
  (dp-delete-line 20)
  (search-forward "\\firstindextab")
  (forward-line -2)
  (dp-delete-line 1)
  (search-forward "\\seeterm\\term")
  (replace-match "\\seeterm" t t)
  (search-forward "\\endlist")
  (dp-delete-line 2)
  (insert "@end table\n")
  (dp-pm)
  (search-forward "\\begincom{deftype}" nil t)
  (save-restriction
    (narrow-to-region (point) (search-forward "\\endcom"))
    (dp-pm)
    (while (re-search-forward "\\$\\\\sub{\\(.\\)}\\$" nil t)
      (replace-match "_\\1")))
  (dp-pm)
  (search-forward "\\DefineSection{DeterminingtheEffectiveMethod" nil t)
  (save-restriction
    (narrow-to-region (point) (search-forward "\\endlist"))
    (dp-pm)
    (while (re-search-forward "\\(\\\\itemitem{..}\\){\\([^}]+\\)}" nil  t)
      (replace-match "\\1 \\2")))
  (dp-pm)
  (search-forward "unbound-slot-object" nil t)
  (replace-match "unbound-slot-instance")
  (dp-pm)
  (search-forward "\\chapref\\ReaderConcepts" nil t)
  (replace-match "\\secref\\ReaderConcepts" t t)
  (dp-pm)
  (search-forward "{\\tt ~*}" nil t)
  (replace-match "@tt{ @tild{}*}")
  (dp-pm)
  (search-forward "\\misc{t} (\\term{constant variable})")
  (replace-match "\\conref{t}" t t)
  (dp-pm)
  (search-forward "\\misc{nil} (\\term{constant variable})")
  (replace-match "\\conref{nil}" t t))

(defun dp-remove-anchors ()
  "Remove unused anchors."
  (let (refs anchors)
    (dp-pm)
    (search-forward "@anchor{1-}")
    (replace-match "@anchor{1- (Function)}")
    ;; this anchor is referenced in dp.texi
    (push "SatisfyingTheTwoArgTest" refs)
    (dp-pm)
    (while (re-search-forward "@ref{\\([^},]+\\)" nil t)
      (push (match-string 1) refs))
    (setq anchors (set-difference dp-anchors refs :test 'string=))
    (dolist (anchor anchors)
      (dp-pm)
      (when (search-forward (concat "@anchor{" anchor "}") nil t)
	(replace-match "")
	(when (eolp)
	  (delete-char 1))))))

(defun dp-auxbnf (command)
  (replace-match (concat "@" command) t t nil)
  (save-excursion
    (insert
     (dp-remove-whitespace
      (dp-strip-newline
       (concat
	"{" (dp-quote-comma (dp-get-arg-delete)) ", "
	(dp-quote-comma (dp-get-arg-delete)) "}")
       t)))))

(defun dp-setup ()
  (setq dp-chapter-list nil
	dp-chapter-name-alist nil
	dp-dictionary-p nil
	dp-anchors nil
	dp-nodes nil
	dp-current-fig-label nil
	dp-section-no 0
	dp-section-names nil
	dp-current-label nil
	dp-subsection-no 0
	dp-subsections-list nil)
  (dp-defconvert 'dp-brace-command
		 'rest 'opt 'keyword 'tt 'bf 'prmseven)
  (dp-defconvert 'dp-f 'f)
  (dp-defconvert 'dp-auxbnf 'auxbnf))

(defvar dp-tr-alist
  '(("Õ" . "≡") ("Ö" . "▷") ("Ø" . "₀") ("Ù" . "₉") ("Ú" . "₈") ("Û" . "₇")
    ("Ü" . "₆")
    ("Ý" . "₅")
    ("Þ" . "₄") ("ß" . "₃") ("à" . "₂") ("á" . "₁") ("â" . "≠") ("ã" . "≤")
    ("ä" . "ː") ("å" . "ō") ("æ" . "ē") ("ç" . "ā") ("è" . "ə") ("é" . "ˌ")
    ("ê" . "ˈ") ("ë" . "·") ("ì" . "α") ("í" . "ε") ("î" . "π") ("ï" . "∂")
    ("ð" . "↓") ("ñ" . "〉") ("ò" . "〈") ("ó" . "≤") ("õ" . "⋃") ("ö" . "≥")
    ("÷" . "∈") ("ø" . "〛") ("ù" . "〚") ("ú" . "⁺") ("û" . "↓") ("ô" . "↩")
    ("ý" . "→") ("þ" . "’") ("ÿ" . "‘")))

(defun dp-tr ()
  "Map 8bit values in Info files to multibyte chars."
  (interactive)
  (let ((re (concat "[" (mapconcat (lambda (x) (car x)) dp-tr-alist "")
		    "]"))
	case-fold-search)
    (dolist (file (directory-files default-directory nil "ansicl-?[0-9]*$"))
      (with-temp-buffer
	(let ((coding-system-for-read 'latin-1))
	  (insert-file-contents file))
	(dp-pm)
	(while (re-search-forward re nil t)
	  (replace-match (cdr (assoc (match-string 0)
				   dp-tr-alist)) t t))
	(let ((coding-system-for-write 'utf-8))
	  (write-region (point-min) (point-max) file))))))

(defun dp-convert ()
  "Main translation loop."
  (let (command)
    (while (re-search-forward "\\\\\\(\\w+\\)" nil 'move-to-limit)
      (setq command (match-string 1))
      (funcall (get (intern command) 'convert) command))))

(defun dp-tex2texi ()
  "Convert TeX sources to Texinfo and save in the file 'temp.texi'."
  (interactive)
  (setq dp-work-buffer (get-buffer-create " *dp-work*"))
  (with-current-buffer dp-work-buffer
    (set-syntax-table dp-syntax-table))
  (with-temp-buffer
    (with-syntax-table dp-syntax-table
      (buffer-disable-undo)
      (erase-buffer)
      (insert-file-contents "chap-0-edit-history.tex")
      (dp-strip-comments)
      (dotimes (i 26)
       (goto-char (point-max))
       (insert-file-contents (format "chap-%d.tex" (1+ i))))
      (goto-char (point-max))
      (insert-file-contents "chap-a.tex")
      ;; conversion
      (dp-parse-macros "dp.texi")
      (dp-setup)
      (dp-get-section-names)
      (dp-insert)
      (dp-quote-special)
      (dp-special-cases)
      (dp-hack-credits)
      (dp-non-code)
      (dp-pm)
      (dp-convert)
      ;; post-process
      (dp-pm)
      (dp-hack-curly)
      (dp-hack-~)
      (dp-remove-anchors)
      (dp-pm)
      (insert dp-preamble)
      (push "Index" dp-chapter-list)
      (push "Symbol Index" dp-chapter-list)
      (push "List of Figures" dp-chapter-list)
      (setq dp-chapter-list (nreverse dp-chapter-list))
      (dolist (node dp-chapter-list)
	(insert "* " node "::\n"))
      (insert "@ifnotinfo\n"
	      "* Table of Contents::\n"
	      "@end ifnotinfo\n"
	      "@end menu\n")
      (goto-char (point-max))
      (insert dp-postamble)
      (setq dp-chapter-list nil))
    (write-region (point-min) (point-max) "temp.texi")))

;;; Local Variables: ***
;;; mode:emacs-lisp ***
;;; coding:utf-8 ***
;;; End: ***
;;; dpans2texi.el ends here
