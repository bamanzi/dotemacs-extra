;;; abs-indent-mode.el --- mimic other simple editor's indent/unindent

;; Copyright (C) 2012 - 2015, by Ba Manzi

;; Author: Ba Manzi <ba.manzi@gmail.com>
;; URL: https://bitbucket.org/bamanzi/dotemacs-extra/src/default/editing
;; Package-Requires: 
;; Keywords: indent

;; This file is part of GNU Emacs.

;;; Commentary:

;; unlike emacs' default settings, this would not use syntax-based indent, but:
;;
;;  - if region selected, indent/unindent the region (tab-width)
;;    * the region mark would not deactivated automatically
;;
;;  - if no region selected, <TAB> would
;;    * if cursor lies in line leading, always indent by tab-width
;;    * if cursor lies in word ending and `tab-always-indent' is `complete', try complete
;;    * otherwise, always insert a TAB char or SPACEs
;;
;;  - if no region selected, <S-TAB> would
;;    * if cursor lies in line leading, always unindent by tab-width
;;    * otherwise, the cursor would move backwards (tab-width)
;;
;; Note: this implementation would hornor `tab-always-indent',
;; `indent-tabs-mode' and `tab-with'.

;;; Code:
(defvar abs-indent-complete-function 'dabbrev-expand
  "The function used in `abs-indent' for completion.")
(make-variable-buffer-local 'abs-indent-complete-function)
  
(defun abs-indent (arg)
  "Absolutely indent current line or region. Mimic other editors' indent."
  (interactive "P")
  (let ( (width (or arg tab-width)) )
  (if mark-active
      ;;DONE: how to restore region after `indent-rigidly'
      (let ( (deactivate-mark nil) )
        (indent-rigidly (region-beginning) (region-end) width))
    (let ( (pt           (point))
           (pt-bol       (line-beginning-position))
           (pt-bol-nonws (save-excursion (back-to-indentation) (point))) )
      (if (<= pt pt-bol-nonws)  ;;in leading whitespaces
          (progn
            (back-to-indentation)
            (if (looking-at "$")  ;;all chars in this line are whitespaces or tabs
                  (indent-to (+ (current-column) width))
                (progn
                  (indent-rigidly pt-bol (line-end-position) width)
                  (back-to-indentation))))
        (if (and (eq tab-always-indent 'complete)
                 (looking-at "\\>"))
            (call-interactively abs-indent-complete-function)
          (if indent-tabs-mode
              (insert-char ?\t 1)
            (insert-char ?  width))))))))
  
(defun abs-unindent (arg)
  "Absolutely unindent current line or region."
  (interactive "P")
  (if mark-active
      (let ( (deactivate-mark nil) )
        (indent-rigidly (region-beginning) (region-end) (- tab-width)))
    (let ( (pt           (point))
           (pt-bol       (line-beginning-position))
           (pt-bol-nonws (save-excursion (back-to-indentation) (point))) )
      (if (> pt pt-bol-nonws)  ;;in content
          (move-to-column (max 0 (- (current-column) tab-width)))
        (progn
          (back-to-indentation)
          (backward-delete-char-untabify (min tab-width (current-column))))))))
 
 (defvar abs-indent-mode-map
  (let ( (map (make-sparse-keymap)) )
    (define-key map "\t"        'abs-indent) 
    (define-key map [S-tab]     'abs-unindent)
    map)
  "keymap for `abs-indent-mode'.")

(define-minor-mode abs-indent-mode
  "simple indent just like other editors."
  nil
  " ai"
  abs-indent-mode-map
  (if abs-indent-mode
      t
    t))

;;; abs-indent-mode.el ends here
