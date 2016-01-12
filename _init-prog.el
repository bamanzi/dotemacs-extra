;; ** yasnippet
(autoload 'yas/global-mode "yasnippet"
  "Toggle Yas minor mode in all buffers." t)

(autoload 'yas/minor-mode "yasnippet"
  "Toggle YASnippet mode." t)

(autoload 'anything-yasnippet-2 "anything-c-yasnippet-2"
  "Yasnippet from `anything'." t)

(global-set-key (kbd "<f5> s")  'anything-yasnippet-2)


;; ** completion
(eval-after-load "auto-complete-config"
  `(if (load "auto-complete-scite-api" t)
       (add-to-list 'ac-sources 'ac-source-scite-api)
     (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))

;;--
(idle-require 'ido-at-point)

(eval-after-load "ido-at-point"
  `(progn
     (ido-at-point-mode t)))


;; ** simple-call-tree
(autoload 'sct-graphviz "sct-graphviz"
  "Analyze the simple tree call and display it as graphic." t)

(eval-after-load "sct-graphviz"
  `(when (eq system-type 'windows-nt)
     (setq sct-graphviz-dir temporary-file-directory)

     (setenv "PATH" (concat "e:\\tools\\graphviz;" (getenv "PATH")))
     (add-to-list 'exec-path "e:/tools/graphviz")
     ;;(executable-find "dot.exe")
     ))

;; ** tags

;; *** find-file-in-tags
(autoload 'find-file-in-tags "find-file-in-tags"
  "find file in TAGS file." t)

(global-set-key (kbd "<f9> . f") 'find-file-in-tags)


;; ** highlighting
(autoload 'color-identifiers-mode "color-identifiers-mode"
  "Color the identifiers in the current buffer based on their names." t)


;;** super+click to jump to declaration/implementation
(defvar symbol-jump/function-alist
  '((emacs-lisp-mode       . symbol-jump/elisp-jump-to-symbol)
    (lisp-interaction-mode . symbol-jump/elisp-jump-to-symbol)
    (help-mode             . symbol-jump/elisp-jump-to-symbol)
    (c-mode                . semantic-complete-jump)
    (java-mode             . semantic-complete-jump)
    (python-mode           . semantic-complete-jump)
    (ruby-mode             . robe-jump))
  "functions used in `symbol-jump' for each major mode.

Each function should accept ONE argument in STRING type.")

(idle-require 'pulse)

(defun symbol-jump ()
  (interactive)
  (let* ( (symbol (thing-at-point 'symbol))
          (symbol-text (progn
                         (set-text-properties 0 (length symbol) nil symbol)
                         symbol))
          (jump-method (assoc major-mode symbol-jump/function-alist)) )
    (if jump-method
        (progn
          (message "call `%s' to jump to `%s'" jump-method symbol-text)
          (apply jump-method (cdr symbol-text))
          (when (and pulse-command-advice-flag (interactive-p))
            (pulse-momentary-highlight-one-line (point))))
      (message "no configuration for `%s' in `symbol-jump/function-alist'." major-mode))))

(global-set-key (kbd "s-j") 'symbol-jump)

;; *** jump functions
(defun symbol-jump/imenu-goto-symbol (foo)
  (interactive )
  (imenu (thing-at-point 'symbol)))

(defun symbol-jump/elisp-jump-to-symbol (symbol-text)
  (interactive (list ((read-string "Symbol: "
                                   (thing-at-point 'symbol)))))
  (require 'bmz-elisp-misc)
  (find-symbol-at-point (intern-soft symbol-text)))

(defun symbol-jump/elisp-describe-symbol (symbol-text)
  (require 'bmz-elisp-misc)
  (describe-symbol-at-point  (intern-soft symbol-text)))

;; *** mouse
(defun symbol-jump/on-mouse-down (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ( (begin  (save-excursion (re-search-backward "\\_<") (point)))
          (end    (save-excursion (re-search-forward  "\\_>") (point)))
          (symbol (buffer-substring begin end))
          (ov     (make-overlay begin end)) )
    (overlay-put ov 'category "symbol-jump")
    (overlay-put ov 'face '(:underline t)    )
  ))

(defun symbol-jump/remove-overlay (point)
  (let ( (overlays (overlays-at (point))) )
    (mapcar '(lambda (ov)
               (if (string= (overlay-get ov 'category) "symbol-jump")
                   (delete-overlay ov)))
            overlays
            )
         ))

(defun symbol-jump/on-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (symbol-jump/remove-overlay (point))
  (call-interactively 'symbol-jump))
  

(define-key global-map (kbd "<s-down-mouse-1>") 'symbol-jump/on-mouse-down)
(define-key global-map (kbd "<s-mouse-1>")      'symbol-jump/on-mouse)

(define-key global-map (kbd "<s-mouse-3>")      'pop-to-mark-command)
(define-key global-map (kbd "<C-s-mouse-3>")    'pop-global-mark)

;; ** realgud
;;https://github.com/rocky/emacs-dbgr

(add-to-list 'load-path "~/.emacs.d/packages/realgud")

(autoload 'realgud-gdb    "realgud" nil t)
(autoload 'realgud-pdb    "realgud" nil t)
(autoload 'realgud-perldb "realgud" nil t)
(autoload 'realgud-rdebug "realgud" nil t)

(defun load-realgud ()
     (interactive)

     (require 'realgud)

     (autoload 'pdb            "realgud" nil t)
     (autoload 'perldb         "realgud" nil t)
     (autoload 'rdebug         "realgud" nil t)

     (autoload 'realgud-track-mode "realgud" nil t)
     )
