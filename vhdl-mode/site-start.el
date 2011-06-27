;;;
;;; VHDL Mode
;;;

(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t)

(setq auto-mode-alist (cons '("\\.vhdl?$" . vhdl-mode) auto-mode-alist))

;;; Customizations for VHDL Mode

(custom-set-variables
 ;; enter customizations of VHDL Mode variables here
 )


;;;
;;; Miscellaneous customizations
;;;

(custom-set-variables
 '(ps-paper-type 'a4)
 '(ps-print-color-p nil)
 '(show-paren-mode t nil (paren))
 )
