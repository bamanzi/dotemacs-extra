;; ** yasnippet
(autoload 'anything-yasnippet-2  "anything-yasnippet-2"
  "Find yasnippets." t)

(global-set-key (kbd "<f5> s")  'anything-yasnippet-2)

;; ** scite-api
(eval-after-load "auto-complete-config"
  `(if (load "auto-complete-scite-api" t)
       (add-to-list 'ac-sources 'ac-source-scite-api)
     (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))



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




;; ** rectangle

(autoload 'rectplus-copy-rectangle  "rect+"
  "Copy rectangle area." t)
(autoload 'rectplus-insert-number-rectangle  "rect+"
  "Insert incremental number into each left edges of rectangle's line." t)

(define-key ctl-x-r-map (kbd "M-w") 'rectplus-copy-rectangle)
(define-key ctl-x-r-map (kbd "M-n") 'rectplus-insert-number-rectangle)


;; ** back-button: Visual navigation through mark rings
;;https://github.com/rolandwalker/back-button
(if (and (< emacs-major-version 24)
         (locate-library "smartrep"))
    (message "WARNING: smartrep isn't compatible with emacs < 23 yet. you should remove it: %s"
             (locate-library "smartrep"))
  (idle-require 'back-button))

(eval-after-load "back-button"
  `(progn
     (back-button-mode 1)
     (define-key goto-map (kbd "<left>")    'back-button-local-backward)
     (define-key goto-map (kbd "<right>")   'back-button-local-backward)
     (define-key goto-map (kbd "<M-left>")  'back-button-global-backward)
     (define-key goto-map (kbd "<M-right>") 'back-button-global-backward)

     ))

;; *** recent-jump
;;(idle-require 'recent-jump)

(autoload 'recent-jump-mode "recent-jump"
  "Toggle recent-jump mode." t)

(setq rj-column-threshold 100)
(eval-after-load "recent-jump"
  `(progn
     (define-key goto-map (kbd "<left>")  'recent-jump-backward)
     (define-key goto-map (kbd "<right>") 'recent-jump-forward)
     ))


;; ** smart-mode-line
(if (>= emacs-major-version 24)
    (idle-require 'smart-mode-line)
  )

(autoload 'sml/setup "smart-mode-line"
  "Setup the mode-line, or revert it." t)

(eval-after-load "smart-mode-line"
  `(progn
     (sml/setup)
     (setq sml/hidden-modes '(" RJ"
                              " drag"
                              " back"
                              " org-link"
                              " Hi"
                              " -Chg"
                              " Undo-Tree"))

     (setq-default mode-line-format
                   (cons " "
                         (cons '(which-func-mode which-func-format)
                                (default-value 'mode-line-format))))
     ))


;; ** hide some lines
(autoload 'hide-matching-lines "hide-lines"
  "Hide lines matching the specified regexp." t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)


;; ** ibuffer-vc

(eval-after-load "ibuffer-vc"
  `(progn     
     (add-hook 'ibuffer-hook
               (lambda ()
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 (unless (eq ibuffer-sorting-mode 'alphabetic)
                   (ibuffer-do-sort-by-alphabetic))))

     (setq ibuffer-formats
           '((mark modified read-only vc-status-mini " "
                   (name 18 18 :left :elide)
                   " "
                   (size 9 -1 :right)
                   " "
                   (mode 16 16 :left :elide)
                   " "
                   (vc-status 16 16 :left)
                   " "
                   filename-and-process)))     
     ))


;; ** vlf
(autoload 'vlf "vlf"  "View Large FILE." t)

(idle-require 'vlf)


;; ** desktop-registry
(autoload 'desktop-registry-change-desktop  "desktop-registry"
  "Change to the desktop named NAME." t)

(global-set-key (kbd "<f12> <f12>") 'desktop-registry-change-desktop)

(idle-require 'desktop-registry)

(eval-after-load "desktop-registry"
  `(progn
     (desktop-registry-auto-register 1)
     ))

(unless (fboundp 'file-name-base)
  (defun file-name-base (&optional filename)
    "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
    (file-name-sans-extension
     (file-name-nondirectory (or filename (buffer-file-name))))))

(require 'cl-lib)
(unless (fboundp 'cl-find)
  (defalias 'cl-find 'find))


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

;; ** popwin
(defun popwin-mode ()
  (interactive)
  (if display-buffer-function
      ;; turn off
      (progn
        (setq display-buffer-function nil)
        (message "popwin deactivated."))
    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)
    (global-set-key (kbd "<f11> `") popwin:keymap)
    (message "popwin activated.")
    ))

(global-set-key (kbd "<f11> `") 'popwin-mode)
    

;; ** misc
(autoload 'yagist-list "yagist"
  "Displays a list of all of the current user's gists in a new buffer." t)

(idle-require 'volatile-highlights)

(when (eq system-type 'windows-nt)
  (idle-require 'w32-browser))

(idle-require 'dired+)

(idle-require 'buff-menu+)
(eval-after-load "buff-menu+"
  `(progn
     (load-library "buff-menu")))

;;(idle-require 'vc+) ;;disabled as it's buggy

(idle-require 'menu-bar+)

(idle-require 'info+)

(idle-require 'mouse3)

