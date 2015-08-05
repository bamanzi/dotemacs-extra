;;; ** go-to
(autoload 'avy-goto-char-2 "avy"
  "Jump to the currently visible CHAR1 followed by CHAR2." t)

(autoload 'avy-goto-line "avy"
  "Jump to a line start in current buffer." t)

(global-set-key (kbd "M-g c") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)

;; ** search
(autoload 'swiper "swiper"
  "`isearch' with an overview." t)

(global-set-key (kbd "<apps> C-s") 'swiper)


;; ** folding
;; *** outline
(autoload 'outline-cycle  "outline-magic"
  "Visibility cycling for outline(-minor)-mode." t)
(autoload 'outline-move-subtree-up  "outline-magic"
  "Move the currrent subtree up past ARG headlines of the same level." t)
(autoload 'outline-move-subtree-down  "outline-magic"
  "Move the currrent subtree down past ARG headlines of the same level." t)

(eval-after-load "outline-cycle"
  `(progn
     (define-key outline-mode-prefix-map (kbd "TAB") 'outline-cycle)
     ))


;; *** other folding
(autoload 'hide-region-hide  "hide-region"
  "Hides a region by making an invisible overlay over it and save the" t)
(autoload 'hide-region-unhide  "hide-region"
  "Unhide a region at a time, starting with the last one hidden and" t)


(autoload 'hide-matching-lines "hide-lines"
  "Hide lines matching the specified regexp." t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)

;; ** outshine = outline + org-mode
;;TAB key for org-mode like folding

(eval-after-load "outshine"
  `(progn
     ;;(add-hook 'outline-minor-mode-hook 'outshine-hook-function)

     (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1)
     (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2)
     (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3)
     (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
     (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
     (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)     
     ))

;; outorg is like "reverse Org-Babel":
;; call `outorg-edit-as-org' (C-z ') to edit outline(outshine) section in `org-mode',
;; and `outorg-copy-edits-and-exit' (M-#) to convert back.

(autoload 'outorg-edit-as-org "outorg"
  "Convert and copy to temporary Org buffer" t)

(eval-after-load "outorg"
  `(progn
     (load-library "outshine")
     ))





;; ** rectangle
;; *** C-x r ...
(autoload 'rectplus-copy-rectangle  "rect+"
  "Copy rectangle area." t)
(autoload 'rectplus-insert-number-rectangle  "rect+"
  "Insert incremental number into each left edges of rectangle's line." t)

(define-key ctl-x-r-map (kbd "M-w") 'rectplus-copy-rectangle)
(define-key ctl-x-r-map (kbd "M-n") 'rectplus-insert-number-rectangle)

;; *** cua rectangle (visual)



;; ** mark-thing
(autoload 'mark-thing "thing-cmds"
  "Set point at one end of THING and set mark ARG THINGs from point." t)

(global-set-key (kbd "C-x `") 'mark-thing)


;; this one works for THING without `forward-op' (such as number, string)
;; but it lacks continous marking capability (it can't mark several things)
(defun mark-thing/my (thing)
  (interactive (list
                (intern (completing-read "Thing (type): " (thgcmd-things-alist) nil nil nil nil
                                         (symbol-name thgcmd-last-thing-type)))))
  (if (stringp thing)
      (setq thing (intern thing)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (goto-char (car bounds))
      (push-mark (cdr bounds) nil transient-mark-mode)
      (setq deactivate-mark nil))))

