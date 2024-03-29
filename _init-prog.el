;; ** indent
;; *** aggressive-indent-mode
(autoload 'aggressive-indent-mode "aggressive-indent"
  "Toggle Aggressive-Indent mode on or off." t)

(eval-after-load "lisp-mode"
  `(progn
     ;;     (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode 'append)
     (add-hook 'lisp-interaction-mode-hook 'aggressive-indent-mode 'append)
     ))

(global-set-key (kbd "<f10> a i") 'aggressive-indent-mode)

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



;; ** yasnippet
(autoload 'yas/global-mode "yasnippet"
  "Toggle Yas minor mode in all buffers." t)

(autoload 'yas/minor-mode "yasnippet"
  "Toggle YASnippet mode." t)

(autoload 'anything-yasnippet-2 "anything-c-yasnippet-2"
  "Yasnippet from `anything'." t)

(global-set-key (kbd "M-o s")  'anything-yasnippet-2)


;; ** completion
(eval-after-load "auto-complete-config"
  `(if (load "auto-complete-scite-api" t)
       (add-to-list 'ac-sources 'ac-source-scite-api)
     (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))

;;--
(try-require 'ido-at-point)

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

(autoload 'helm-simple-call-tree "helm-call-tree"
  "Preconfigured `helm' for simple-call-tree. List function relationships." t)

(cheatsheet-add :group 'Programming
                  :key "M-x sct-graphviz"
                  :description "Analyze the simple tree call and display it as graphic.")
(cheatsheet-add :group 'Programming
                  :key "M-x helm-simple-call-tree"
                  :description "Show function call relationships with Helm interface.")


;; ** tags

;; *** helm-etags+ / anything-etags+
;; compared with `anything-c-etags-select' from anything-config.el, `helm-etags+' has some advantages:
;; - it hornors `tags-file-name' (while `anything-c-etags-select' only supports TAGS file along the path)
;; - it supports multiple TAGS file (i.e. `tags-table-list')
;; - we can go forward in history
;; but `helm-etags+' has its own history ring, `helm-etags+history-go-back' is not fully compatible with `pop-tag-mark'

(autoload 'helm-etags+-select "helm-etags+"
  "Find Tag using `etags' and `helm'" t)

(eval-after-load "helm-etags+"
  `(progn
     (global-set-key (kbd "M-o M-.")   'helm-etags+-select)
     (global-set-key (kbd "M-o . SPC") #'(lambda ()
                                            (interactive)
                                            (helm-etags+-select-internal "")))

     ;; 'helm-etags+-history' has its own marker-ring. thus could
     ;; not be used with `find-tag'
     (global-set-key (kbd "M-o .  M-h") 'helm-etags+-history)

     (global-set-key (kbd "M-o . <") 'helm-etags+-history-go-back)
     (global-set-key (kbd "M-o . >") 'helm-etags+-history-go-forward)
     ))

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-o M-."
                     :description "helm-etags+-select-at-point (or anything-...)")
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-o . SPEC"
                     :description "helm-etags+-select (or anything-...)")
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-o . M-h"
                     :description "helm-etags+-history (or anything-...)")
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-o . <"
                     :description "helm-etags+-history-go-back (or anything-...)")
     (cheatsheet-add :group 'Programming/Tags
                     :key "M-o . >"
                     :description "helm-etags+-history-go-forward (or anything-...)")
     t))


;; **** for emacs<=23 (helm requires emacs>=24.3)
(autoload 'anything-etags+-select-at-point "anything-etags+"
  "Tag jump with current symbol using etags and `anything'." t)
(autoload 'anything-etags+-select "anything-etags+"
  "Tag jump using etags and `anything'." t)

(eval-after-load "anything-etags+"
  `(progn
     (global-set-key (kbd "M-o M-.")   'anything-etags+-select-at-point)
     (global-set-key (kbd "M-o . SPC") 'anything-etags+-select)

     ;; 'anything-etags+-history' has its own marker-ring. thus could
     ;; not be used with `find-tag'
     (global-set-key (kbd "M-o .  M-h") 'anything-etags+-history)

     (global-set-key (kbd "M-o . <") 'anything-etags+-history-go-back)
     (global-set-key (kbd "M-o . >") 'anything-etags+-history-go-forward)
     ))


;; *** find-file-in-tags
(autoload 'find-file-in-tags "find-file-in-tags"
  "find file in TAGS file." t)

(cheatsheet-add :group 'Programming/Tags
                :key "M-x find-file-in-tags"
                :description "Find file in TAGS file")


;; ** highlighting
(autoload 'color-identifiers-mode "color-identifiers-mode"
  "Color the identifiers in the current buffer based on their names." t)

(global-set-key (kbd "<f10> c i") 'color-identifiers-mode)

(cheatsheet-add :group 'Programming
                :key "<f10> c i"
                :description "color-identifiers-mode")


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

(try-idle-require 'pulse)

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

;; ** flymake/flycheck
;; *** flycheck
(autoload 'flycheck-mode "flycheck"
  "Flymake reloaded with useful checkers. " t)

;;load flycheck rather than flymake
;;(idle-require 'flycheck)

(global-set-key (kbd "<M-f9>") 'flycheck-mode)

(define-key global-map (kbd "<f9> M-n") 'flymake-goto-next-error)
(define-key global-map (kbd "<f9> M-p") 'flymake-goto-prev-error)

;;fix endless loop bug of `flycheck-find-file-in-tree' on Windows
(eval-after-load "flycheck"
  `(progn
     (defun flycheck-find-file-in-tree (filename directory)
       "Find FILENAME in DIRECTORY and all of its ancestors.

Start looking for a file named FILENAME in DIRECTORY and traverse
upwards through all of its ancestors up to the file system root
until the file is found or the root is reached.

Return the absolute path of the file, or nil if the file was not
found in DIRECTORY or any of its ancestors."
       (let ((full-path (expand-file-name filename directory)))
         (cond ((or (string= directory "/")
                    (string= ":/" (substring directory 1 3)))
                (when (file-exists-p full-path) full-path))
               ((file-exists-p full-path)
                full-path)
               (t
                (let ((parent-directory (file-name-directory
                                         (directory-file-name
                                          (file-name-directory full-path)))))
                  (flycheck-find-file-in-tree filename parent-directory))))))
     ))

