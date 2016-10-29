;; ** go-to
(autoload 'avy-goto-char-in-line "avy"
  "Jump to the currently visible CHAR in the current line." t)

(autoload 'avy-goto-line "avy"
  "Jump to a line start in current buffer." t)

(global-set-key (kbd "M-g c") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g l") 'avy-goto-line)

(progn
  (cheatsheet-add :group 'Jump
                  :key "M-g c"
                  :description "avy-goto-char-in-line")
  (cheatsheet-add :group 'Jump
                  :key "M-g l"
                  :description "avy-goto-line")
  t
  )

;; *** zop-to-char
(autoload 'zop-up-to-char  "zop-to-char"
  "An enhanced version of `zap-up-to-char'." t)

(global-set-key [remap zap-to-char] 'zop-up-to-char)


;; ** search
;; *** swiper
(autoload 'swiper "swiper"
  "`isearch' with an overview." t)

(global-set-key (kbd "<apps> C-s") 'swiper)

(progn
  (cheatsheet-add :group 'Search
                  :key "<apps> C-s"
                  :description "swiper (`isearch' with an overview.)")
  )


;; *** narrowing
(autoload 'hide-region-hide  "hide-region"
  "Hides a region by making an invisible overlay over it and save the" t)
(autoload 'hide-region-unhide  "hide-region"
  "Unhide a region at a time, starting with the last one hidden and" t)

(autoload 'hide-matching-lines "hide-lines"
  "Hide lines matching the specified regexp." t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)

(progn
  (cheatsheet-add :group 'Search
                  :key "M-x hide-matching-lines"
                  :description "Hide lines matching the specified regexp.")
  (cheatsheet-add :group 'Search
                  :key "M-x hide-non-matching-lines"
                  :description "Hide lines that don't match the specified regexp.")
  t
  )

;; *** regexp search/replace
(autoload 'vr/isearch-forward "visual-regexp-steroids"
  "Like isearch-forward, but using Python (or custom) regular expressions." t)
(autoload 'vr/isearch-backward "visual-regexp-steroids"
  "Like isearch-backward, but using Python (or custom) regular expressions." t)

(defun vr/isearch-forward/emacs ()
  "Call `vr/isearch-forward' with `vr/engine' set to 'emacs."
  (interactive)
  (require 'visual-regexp-steroids)
  (let ((vr/engine 'emacs))
    (call-interactively 'vr/isearch-forward)))

(defun vr/isearch-forward/python ()
  "Call `vr/isearch-forward' with `vr/engine' set to 'python."
  (interactive)
  (require 'visual-regexp-steroids)
  (let ((vr/engine 'python))
    (call-interactively 'vr/isearch-forward)))

;; ---
(autoload 'vr/query-replace "visual-regexp"
  "Use vr/query-replace like you would use query-replace-regexp." t)

(defun vr/query-replace/emacs ()
  "Call `vr/query-repalce' with `vr/engine' set to #'emacs."
  (interactive)
  (require 'visual-regexp-steroids)
  (let ((vr/engine 'emacs))
    (call-interactively 'vr/query-replace)))

(defun vr/query-replace/python ()
  "Call `vr/query-repalce' with `vr/engine' set to #'python."
  (interactive)
  (require 'visual-regexp-steroids)
  (let ((vr/engine 'python))
    (call-interactively 'vr/query-replace)))

;; ---
(try-idle-require 'visual-regexp-steroids)
(eval-after-load "visual-regexp-steroids"
  `(progn
     (setq vr/engine 
           (if (executable-find "python")
               'python
             'emacs))

     (global-set-key [remap isearch-forward-regexp]      'vr/isearch-forward)
     (global-set-key [remap isearch-backward-regexp]     'vr/isearch-backward)
     ))

(progn
  (cheatsheet-add :group 'Search
                  :key "M-x vr/isearch-forward"
                  :description "`isearch-forward' with Python (or custom) regular expressions if available.")
  (cheatsheet-add :group 'Search
                  :key "M-x vr/isearch-backward"
                  :description "`isearch-backward' with Python (or custom) regular expressions if available.")
  
  (cheatsheet-add :group 'Search
                  :key "M-x vr/query-replace/emacs"
                  :description "`query-replace-regexp' with interactive visual feedback, using emacs regexp.")
  (cheatsheet-add :group 'Search
                  :key "M-x vr/query-replace/python"
                  :description "`query-replace-regexp' with interactive visual feedback, using python regexp.")
  t
  )


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


;; *** outshine = outline + org-mode
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

;; put rectangle-area to `killed-rectangle'
;; (same with `copy-rectangle-as-kill' in emacs>=24.3)
(autoload 'rectplus-copy-rectangle  "rect+"
  "Copy rectangle area." t)
;; copy content of `killed-rectangle' to `kill-ring'
(autoload 'rectplus-rectangle-to-kill-ring "rect+"
  "Killed rectangle to normal `kill-ring'." t)

(autoload 'rectplus-insert-number-rectangle  "rect+"
  "Insert incremental number into each left edges of rectangle's line." t)

(define-key ctl-x-r-map (kbd "M-n")   'rectplus-insert-number-rectangle)

(progn
  (cheatsheet-add :group 'Rectangle
                  :key "M-x rectplus-copy-rectangle"
                  :description "Copy rectang-area to `killed-rectangle'.")
  (cheatsheet-add :group 'Rectangle
                  :key "M-x rectplus-rectangle-to-kill-ring"
                  :description "Copy content of `killed-rectangle' to `kill-ring'")
  t
  )


;; *** cua rectangle (visual)
(autoload 'cua-mouse-set-rectangle-mark "cua-rect"
  "Start rectangle at mouse click position." t)

;; *** rectangle-mark-mode (visual, but only available on emacs>=24.4)

;; use mouse to mark rectangle (r-m-m)
;; https://tangjunjie.wordpress.com/2015/07/10/enable-emacs-column-selection-using-mouse/
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))

(if (fboundp 'rectangle-mark-mode)
    (global-set-key (kbd "<C-M-down-mouse-1>") #'mouse-start-rectangle)
  (global-set-key (kbd "<C-M-down-mouse-1>")   'cua-mouse-set-rectangle-mark))

(progn
  (cheatsheet-add :group 'Rectangle
                  :key "<C-M-down-mouse-1>"
                  :description "mouse-start-rectangle (r-m-m) / cua-mouse-set-rectangle-mark (cua)")
  t
  )


;; ** mark-thing
(autoload 'mark-thing "thing-cmds"
  "Set point at one end of THING and set mark ARG THINGs from point." t)

(global-set-key (kbd "C-x `") 'mark-thing)


;; this one works for THING without `forward-op' (such as number, string)
;; but it lacks continous marking capability (it can't mark several things)
(defun mark-thing/my (thing)
  (interactive (list
                (intern (completing-read "Thing (type): "
                                         (progn
                                           (require 'thing-cmds)
                                           (thgcmd-things-alist))
                                         nil nil nil nil
                                         (symbol-name thgcmd-last-thing-type)))))
  (if (stringp thing)
      (setq thing (intern thing)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (goto-char (car bounds))
      (push-mark (cdr bounds) nil transient-mark-mode)
      (setq deactivate-mark nil))))


;; ** some highlighting
;; *** highlight-thing
;; highlight *all occurence* of a thing
(autoload 'highlight-thing-mode "highlight-thing"
  "Minor mode that highlights things at point" t)

(global-set-key (kbd "<f10> h t") 'highlight-thing-mode)

(eval-after-load "highlight-thing"
  `(progn
     ;; highlight all occurrence of current selection
     (setq highlight-thing-what-thing 'region)

     ;; default value 0.5 is too short, causing high CPU usage sometimes when too much occurrence
     (setq highlight-thing-delay-seconds 1.5)
     ))


(progn
  (cheatsheet-add :group 'Highlighting
                  :key "<f10> h t"
                  :description "highlight all occurrence of a thing (current: region)")
  t
  )


;; *** volatile-highlights
(try-idle-require 'volatile-highlights)
(eval-after-load "volatile-highlights"
  `(progn
     (volatile-highlights-mode t)
     ))

