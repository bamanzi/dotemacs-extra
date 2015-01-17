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


;;** super+click to jump to declaration/implementation
(defun symbol-jump-on-mouse-down (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ( (begin  (save-excursion (re-search-backward "\\_<") (point)))
          (end    (save-excursion (re-search-forward  "\\_>") (point)))
          (symbol (buffer-substring begin end))
          (ov     (make-overlay begin end)) )
    (overlay-put ov 'category "linkify-imenu")
    (overlay-put ov 'face '(:underline t)    )
  ))

(defun symbol-jump-remove-overlay (point)
  (let ( (overlays (overlays-at (point))) )
    (mapcar '(lambda (ov)
               (if (string= (overlay-get ov 'category) "linkify-imenu")
                   (delete-overlay ov)))
            overlays
            )
         ))

(defun symbol-jump-on-mouse-up-imenu ()
  (interactive )
  (symbol-jump-remove-overlay (point))
  (imenu (thing-at-point 'symbol))
  (when (and pulse-command-advice-flag (interactive-p))
    (pulse-momentary-highlight-one-line (point)))
  )

(defun symbol-jump-on-mouse-up-among-files ()
  (interactive )
  (symbol-jump-remove-overlay (point))
  (let ( (symbol (thing-at-point 'symbol)) )
    ;; (message "go to symbol: %s" symbol)
    (cond
     ( (memq major-mode '(emacs-lisp-mode lisp-interaction-mode help-mode))
       (if (fboundp (intern symbol))
           (find-function-at-point)
         (find-variable-at-point)))
     ( (and 'semantic-mode
            (memq major-mode '(c-mode java-mode python-mode)))
       (call-interactively 'semantic-complete-jump) )
     (t
      (call-interactively 'find-tag)))))

(define-key global-map (kbd "<s-down-mouse-1>") 'symbol-jump-on-mouse-down)
(define-key global-map (kbd "<s-mouse-1>")      'symbol-jump-on-mouse-up-imenu)
(define-key global-map (kbd "<C-s-mouse-1>")    'symbol-jump-on-mouse-up-among-files)
(define-key global-map (kbd "<s-mouse-3>")      'pop-to-mark-command)
(define-key global-map (kbd "<C-s-mouse-3>")    'pop-global-mark)
