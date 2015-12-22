
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



;; ** desktop-registry
(autoload 'desktop-registry-change-desktop  "desktop-registry"
  "Change to the desktop named NAME." t)

(global-set-key (kbd "<f12> C-l") 'desktop-registry-change-desktop)

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



;; ** vi emulation (viper)

;; *** viper/vimpulse addons
(eval-after-load "vimpulse"
  `(progn
     (require 'vimpulse-cjk nil t)
     (require 'vimpulse-textobj-between nil t)
     (require 'vimpulse-surround nil t)
     ))



;; ** scratch buffer
;; *** pretty print for emacs-lisp
(idle-require 'ipretty)
(eval-after-load "ipretty"
  `(progn
     (define-key emacs-lisp-mode-map [remap eval-print-last-sexp] 'ipretty-last-sexp)
     (define-key lisp-interaction-mode-map [remap eval-print-last-sexp] 'ipretty-last-sexp)
     ))

;; *** log scratch content automatically
(idle-require 'scratch-log)

;; *** scratch buffer for all major modes
(autoload 'scratch  "scratch"
  "Get a scratch buffer for the current mode." t)

(global-set-key (kbd "<f12> M-*") 'scratch)


;; ** indentation guides
;; *** highlight-identation-mode
;; (configured in `dotemacs-elite')
;; cons:
;;    - only whitespaces supported (tabs not supported)

;; *** indent-guide-mode
;;`indent-guide-mode' only show guides on current section.
;; but it would actually use a char (`indent-guide-char') to guide line,
;; thus it might not be suitable for terminal (if you use external copy (mouse or tmux))
(autoload 'indent-guide-mode  "indent-guide"
  "Show vertical lines to guide indentation." t)

(global-set-key (kbd "<f10> ig") 'indent-guide-mode)

;; *** visual-indentation-mode


;; ** minibuffer completion
;; emacs>=24.4's `icomplete-mode' no longer shows keybindings for `M-x',
;; package `icomplete+' reimplemented this feature
(eval-after-load "icomplete"
  `(unless (string< emacs-version "24.4")
     (when (require 'icomplete+ nil t)
       ;; but `icomplete+' disabled the keybinding (C-,/C-. etc) for cycling candidates,
       ;; we need to use `icompletep-cycling-mode' to re-enable it.
       (icompletep-cycling-mode 1))))

;;--
;; enhance `ido'
(eval-after-load "ido"
  `(when (require 'ido-vertical-mode nil t)
     (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

     (add-hook 'ido-setup-hook
               #'(lambda ()     
                   (define-key ido-completion-map (kbd "<up>")   'ido-prev-match)
                   (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
                   (define-key ido-buffer-completion-map (kbd "<up>")   'ido-prev-match)
                   (define-key ido-buffer-completion-map (kbd "<down>") 'ido-next-match)
                   )
               'append)
     
     (ido-vertical-mode t)
     ))

;;-- smex
;; I don't `smex' is better than `icomplete-mode',
;; but `smex-major-mode-commands' is useful
(autoload 'smex-major-mode-commands "smex"
  "Like `smex', but limited to commands that are relevant to the active major mode." t)

(global-set-key (kbd "ESC M-x") 'smex-major-mode-commands)

(setq smex-cache nil
      smex-data nil)  ;;workaround for bug of `smex'


;; ** keys
;; *** which-key/guide-key
(autoload 'guide-key-mode "guide-key"
  "Toggle guide key mode." t)

(if (string< emacs-version "24.3")
    (idle-require 'guide-key)
  (autoload 'which-key-mode  "which-key"
    "Toggle which-key-mode." t)
  (idle-require 'which-key))

(eval-after-load "which-key"
  `(progn
     (which-key-setup-side-window-right-bottom)
     (which-key-mode 1)
     ))

(eval-after-load "guide-key"
  `(progn
     (setq guide-key/guide-key-sequence '("C-x r"
                                          "C-x 4"
                                          "M-g"
                                          "M-s"
                                          "<f11>"
                                          "<f10>"))
     (guide-key-mode 1)  ; Enable guide-key-mode
     ))


;; *** switch off/on touchpad when emacs gets/loses focus
;; https://www.reddit.com/r/emacs/comments/38o0tr/i_have_to_share_this_switch_your_touchpad_off/
(defvar touchpad-device-name nil
  "The name of your touchpad device.

You can figure it out using

xinput --list

e.g. for me it's \"SynPS/2 Synaptics TouchPad\"")

(defun turn-off-mouse (&optional frame)
  (interactive)
  (if touchpad-device-name
      (shell-command (format "xinput --disable \"%s\"" touchpad-device-name))
    (shell-command "synclient TouchpadOff=1")))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (if touchpad-device-name
      (shell-command (format "xinput --enable \"%s\"" touchpad-device-name))
    (shell-command "synclient TouchpadOff=0")))

;; only Emacs >= 24.4 has `focus-in-hook' and `focus-out-hook'
(when (and (boundp 'focus-in-hook)
           (eq window-system 'x)
           (not (string-match "Couldn't find synaptics properties" (shell-command-to-string "synclient -l"))))
  (add-hook 'focus-in-hook #'turn-off-mouse)
  (add-hook 'focus-out-hook #'turn-on-mouse)
  (add-hook 'delete-frame-functions #'turn-on-mouse))


;; ** misc
;;--
(autoload 'yagist-list "yagist"
  "Displays a list of all of the current user's gists in a new buffer." t)

(idle-require 'volatile-highlights)
(eval-after-load "volatile-highlights"
  `(progn
     (volatile-highlights-mode t)
     ))

;;--
;;info+.el: more colors (and other enhancements) 
(eval-after-load "info"
  `(require 'info+)
  )

(eval-after-load "info+"
  `(progn
     (defvar Info-next-link-keymap (make-sparse-keymap))
     (defvar Info-prev-link-keymap (make-sparse-keymap))
     (defvar Info-up-link-keymap   (make-sparse-keymap))
     (defvar Info-down-link-keymap   (make-sparse-keymap))))

;;--
(autoload 'restart-emacs "restart-emacs"
  "Restart Emacs." t)

;;--
(idle-require 'buff-menu+)
(eval-after-load "buff-menu+"
  `(progn
     (load-library "buff-menu")))

;;(idle-require 'vc+) ;;disabled as it's buggy

(idle-require 'menu-bar+)


(idle-require 'mouse3)

;; give `diff-mode' some colors
(idle-require 'diff-mode-)


