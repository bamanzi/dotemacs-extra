
;; ** cross-buffer navigation
;; *** back-button: Visual navigation through mark rings
;;https://github.com/rolandwalker/back-button
(if (and (< emacs-major-version 24)
         (locate-library "smartrep"))
    (message "WARNING: smartrep isn't compatible with emacs < 23 yet. you should remove it: %s"
             (locate-library "smartrep"))
  (try-require 'back-button))

(eval-after-load "back-button"
  `(progn
     (back-button-mode 1)

     (define-key goto-map (kbd "<left>")    'back-button-local-backward)
     (define-key goto-map (kbd "<right>")   'back-button-local-forward)
     (define-key goto-map (kbd "<M-left>")  'back-button-global-backward)
     (define-key goto-map (kbd "<M-right>") 'back-button-global-forward)


     (cheatsheet-add :group 'Jump
                     :key "M-g <left>"
                     :description "back-button-local-backward")
     (cheatsheet-add :group 'Jump
                     :key "M-g <right>"
                     :description "back-button-local-forward")
     (cheatsheet-add :group 'Jump
                     :key "M-g <M-left>"
                     :description "back-button-global-backward")
     (cheatsheet-add :group 'Jump
                     :key "M-g <M-right>"
                     :description "back-button-global-forward")
     t
     ))


;; *** recent-jump
;;(try-idle-require 'recent-jump)

(autoload 'recent-jump-mode "recent-jump"
  "Toggle recent-jump mode." t)

(setq rj-column-threshold 100)
(eval-after-load "recent-jump"
  `(progn
     (define-key goto-map (kbd "<left>")  'recent-jump-backward)
     (define-key goto-map (kbd "<right>") 'recent-jump-forward)

     (cheatsheet-add :group 'Jump
                     :key "M-g <left>"
                     :description "recent-jump-backward")
     (cheatsheet-add :group 'Jump
                     :key "M-g <right>"
                     :description "recent-jump-forward")
     
     ))


;; ** smart-mode-line
(if (>= emacs-major-version 24)
    (try-idle-require 'smart-mode-line)
  (idle-require 'diminish))

(autoload 'sml/setup "smart-mode-line"
  "Setup the mode-line, or revert it." t)

(eval-after-load "smart-mode-line"
  `(progn
     (sml/setup)
     (setq sml/hidden-modes '(" RJ"
                              " drag"
                              " back"
                              " Hi"
                              " org-link"
                              " -Chg"
                              " VHl"
                              " WK"
                              " ivy"
                              " OrgStruct"
                              " Undo-Tree"))

     (setq-default mode-line-format
                   (cons " "
                         (cons '(which-func-mode which-func-format)
                               (remove '(which-func-mode which-func-format)
                                       (default-value 'mode-line-format)))))
     ))

(eval-after-load "diminish"
  `(progn
     (diminish 'drag-stuff-mode)
     (diminish 'highlight-changes-mode)
     (diminish 'hi-lock-mode)
     (diminish 'orgstruct-mode)
     (diminish 'visual-line-mode)
     ))

;; ** vi emulation (viper)

;; *** viper/vimpulse addons
(eval-after-load "vimpulse"
  `(progn
     (require 'vimpulse-cjk nil t)
     (require 'vimpulse-textobj-between nil t)
     (require 'vimpulse-surround nil t)
     ))

;; *** god-mode
(autoload 'god-mode "god-mode"
  "Toggle global God mode." t)
(autoload 'god-local-mode  "god-mode"
  "Minor mode for running commands." t)

(global-set-key (kbd "<f10> g") 'god-local-mode)
(global-set-key (kbd "<f10> G") 'god-mode)


;; ** scratch buffer
;; *** pretty print for emacs-lisp
(try-idle-require 'ipretty)
(eval-after-load "ipretty"
  `(progn
     (define-key emacs-lisp-mode-map [remap eval-print-last-sexp] 'ipretty-last-sexp)
     (define-key lisp-interaction-mode-map [remap eval-print-last-sexp] 'ipretty-last-sexp)
     ))

;; *** log scratch content automatically
(autoload 'sl-restore-scratch "scratch-log"
  "Undocumented." t)
(try-require 'scratch-log)

;; *** page-break-lines
(autoload 'page-break-lines-mode  "page-break-lines"
  "Toggle Page Break Lines mode." t)
(autoload 'global-page-break-lines-mode "page-break-lines"
  "Toggle Page-Break-Lines mode in all buffers." t)

(try-idle-require 'page-break-lines)

(eval-after-load "page-break-lines"
  `(progn
     (let ((scratch (get-buffer "*scratch*")))
       (if scratch
           (with-current-buffer scratch
             (page-break-lines-mode 1))))))


;; ** minibuffer completion

;; *** ivy-mode
;; better replacement for icomplete + ido + ido-vertical + ido-ubiquitous
;; - commands (M-x, where-is)
;; - variables (describe-variable, set-variable, customize-variable, find-variable)
;; - functions (describe-function, find-function)
;; - groups (customize-group)
;; - libraries (find-library)
;; - packages (describe-package, package-install)
;; - tags (find-tag)
;; - info nodes (Info-goto-node, info-lookup-symbol)

(autoload 'ivy-mode "ivy"
  "Toggle Ivy mode on or off." t)

(unless (fboundp 'setq-local)
  ;; emacs <= 24.2 doesn't have `setq-local'
  (defmacro setq-local (var val)
    "Set variable VAR to value VAL in current buffer."
    ;; Can't use backquote here, it's too early in the bootstrap.
    (list 'set (list 'make-local-variable (list 'quote var)) val)))

;;(try-idle-require 'ivy)

(eval-after-load "ivy"
  `(progn
     (icomplete-mode -1)
     (ido-mode -1)

     (ivy-mode 1)
     ))

;; *** icomplete+
;; emacs>=24.4's `icomplete-mode' no longer shows keybindings for `M-x',
;; package `icomplete+' reimplemented this feature
(eval-after-load "icomplete"
  `(unless (string< emacs-version "24.4")
     (when (require 'icomplete+ nil t)
       ;; but `icomplete+' disabled the keybinding (C-,/C-. etc) for cycling candidates,
       ;; we need to use `icompletep-cycling-mode' to re-enable it.
       (icompletep-cycling-mode 1))))

;; *** ido enhancement
;; **** ido-ubiquitous-mode
;; `ido-ubiquitous-mode' is better than `icomplete-mode' (is it?)
(setq ido-everywhere t)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous"
  "Use `ido-completing-read' instead of `completing-read' almost everywhere." t)

(try-idle-require 'ido-ubiquitous)

(eval-after-load "ido-ubiquitous"
  `(progn
     (ido-ubiquitous-mode 1)
     ))

;; **** vertical candidates
(eval-after-load "ido"
  `(when (and (>= emacs-major-version 24)
              (require 'ido-vertical-mode nil t))

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

;; **** smex
;; I don't `smex' is better than `icomplete-mode',
;; but `smex-major-mode-commands' is useful
(autoload 'smex-major-mode-commands "smex"
  "Like `smex', but limited to commands that are relevant to the active major mode." t)

(global-set-key (kbd "ESC M-x") 'smex-major-mode-commands)

(setq smex-cache nil
      smex-data nil)  ;;workaround for bug of `smex'

;; *** cheatsheet
(progn
  (cheatsheet-add :group 'Minibuffer
                  :key "M-x ivy-mode"
                  :description "ivy-mode (emacs>=24)")
  
  (cheatsheet-add :group 'Minibuffer
                  :key "M-x ido-ubiquitous-mode"
                  :description "ido-ubiquitous-mode")
  (cheatsheet-add :group 'Minibuffer
                  :key "M-x ido-vertical-mode"
                  :description "ido-vertical-mode (emacs>=24)")

  (cheatsheet-add :group 'Minibuffer
                  :key "ESC M-x"
                  :description "smex-major-mode-commands")
  
  t)

;; ** keys
;; *** which-key/guide-key
(autoload 'guide-key-mode "guide-key"
  "Toggle guide key mode." t)
(autoload 'which-key-mode "which-key"
  "Toggle which-key-mode." t)

;; (try-idle-require 'which-key) ;; it requires emacs>=24.3
(try-require 'guide-key)

(eval-after-load "which-key"
  `(progn
     (which-key-setup-side-window-right-bottom)
     (which-key-mode 1)
     ))

(eval-after-load "guide-key"
  `(progn
     (setq guide-key/guide-key-sequence '("C-x r"
                                          "C-x v"
                                          "C-x 4"
                                          "C-c C-x"
                                          "C-c @"
                                          "M-g"
                                          "M-s"
                                          "<f1>"
                                          "<f2>"
                                          "<f5>"
                                          "<f6>"
                                          "<f7>"
                                          "<f8>"
                                          "<f9>"
                                          "<f10>"
                                          "<f11>"
                                          "<f12>"
                                          "<apps>"))
     (setq guide-key/highlight-command-regexp
           '("rectangle"
             ("\\(anything\\|helm\\)" . font-lock-keyword-face)
             ("tag" . font-lock-type-face)
             ("bookmark" . "hot pink")))
     (setq guide-key/recursive-key-sequence-flag t)     
     (guide-key-mode 1)  ; Enable guide-key-mode
     ))




;; ** deft (notes manager)
(autoload 'deft  "deft"
  "Switch to *Deft* buffer and load files." t)

(progn
  (setq deft-extensions '("txt" "org" "md"))

  (setq deft-recursive t)
  (setq deft-auto-save-interval 5.0)
  )


;; ** misc
(autoload 'default-text-scale-increase "default-text-scale"
  "Increase the height of the default face by `default-text-scale-amount'." t)
(autoload 'default-text-scale-decrease "default-text-scale"
  "Decrease the height of the default face by `default-text-scale-amount'." t)

(global-set-key (kbd "<C-S-wheel-up>")   'default-text-scale-increase)
(global-set-key (kbd "<C-S-wheel-down>") 'default-text-scale-decrease)

(progn
  (cheatsheet-add :group 'Misc
                  :key "<C-S-wheel-up>"
                  :description "default-text-scale-increase")
  (cheatsheet-add :group 'Misc
                  :key "<C-S-wheel-down>"
                  :description "default-text-scale-decrease")
  t)

;;--
(autoload 'yagist-list "yagist"
  "Displays a list of all of the current user's gists in a new buffer." t)


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

(cheatsheet-add :group 'Misc
                :key "M-x restart-emacs"
                :description "Restart emacs")

;;--

(when (display-graphic-p)
  (try-require 'menu-bar+)
  (try-idle-require 'mouse3))

;; give `diff-mode' some colors
(try-require 'diff-mode-)

