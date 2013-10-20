(setq dotemacs-extra-dir (if load-file-name
                    (file-name-directory load-file-name)
                  default-directory))
(progn
  (add-to-list 'load-path dotemacs-extra-dir)
  (mapc #'(lambda (file)
            (when (and (file-directory-p file)
                       (not (file-exists-p (concat file "/.nosearch"))))
              (message "Prepending %s to load-path" file)
              (add-to-list 'load-path file)
              (let ((default-directory file))
                (normal-top-level-add-subdirs-to-load-path))))
        (directory-files dotemacs-extra-dir 'full "^[a-z][^\\.]+"))

  (if (>= emacs-major-version 24)
      (add-to-list 'custom-theme-load-path (concat dotemacs-extra-dir "themes")))
  )


(if (and (fboundp 'idle-require-mode) idle-require-mode) ;;`idle-require-mode' not finished yet
    (defalias 'idle-require 'require))

    
;;** yasnippet
(autoload 'anything-yasnippet-2  "anything-yasnippet-2"
  "Find yasnippets." t)

(global-set-key (kbd "<f5> s")  'anything-yasnippet-2)

;;** scite-api
(eval-after-load "auto-complete-config"
  `(if (load "auto-complete-scite-api" t)
       (add-to-list 'ac-sources 'ac-source-scite-api)
     (message "%s: failed to load `auto-complete-scite-api'." load-file-name)))

;;** vi emulation (viper)

;;*** viper 
(setq viper-expert-level '3)
(setq viper-inhibit-startup-message 't)

(defun viper-cua-region-fix()
  (define-key viper-vi-global-user-map [backspace] 'backward-delete-char-untabify)
  (define-key viper-vi-global-user-map "\C-d" 'delete-char)
  (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
  (define-key viper-insert-global-user-map "\C-d" 'delete-char))

(eval-after-load "viper" `(viper-cua-region-fix))

;;*** vimpulse
(eval-after-load "viper"
  '(require 'vimpulse))

;;***Ex commands without entering viper-mode
;; stoem from http://www.advogato.org/person/chalst/diary/277.html
;;for ex commands supported by viper, refer `ex-token-alist'

(require 'viper-ex)
(require 'viper-keym)
(require 'viper-cmd)

(define-key global-map (kbd "ESC ESC :") 'viper-ex)

;;** scratch-ext

(require 'scratch-ext nil t)

(defun scratch-save ()
  (interactive)
  (require 'scratch-ext)
  (scratch-ext-save-scratch-to-file)
  )

;;*** set org-mode as major-mode of *scratch*
(progn
  (setq inhibit-startup-screen nil
        initial-scratch-message
        (concat (replace-regexp-in-string ";; " "# " initial-scratch-message)
                "# (from scratch-ext.el: now you can use `scratch-save' to log content of this buffer.)\n\n"))

  (eval-after-load "scratch-ext"
    `(progn

       ;;redefine `scratch-ext-initialize-buffer-as-scratch'
       (defun scratch-ext-initialize-buffer-as-scratch (buffer)
         (with-current-buffer buffer
           (funcall 'org-mode) ;;hack: use `org-mode' as major mode
           (local-set-key (kbd "C-x C-s") 'scratch-save) ;;hack: bind C-x C-s to `scratch-save'
           (local-set-key (kbd "C-c i")   'scratch-ext-insert-newest-log) ;;hack
           (erase-buffer)
           (if (and initial-scratch-message
                    (not inhibit-startup-screen))
               (insert initial-scratch-message))))
       ))
  )


;;** tempbuf: kill unused buffers in the background
(idle-require 'tempbuf)
(eval-after-load "tempbuf"
  `(progn
     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'anything-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'diff-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'completion-list-mode 'turn-on-tempbf-mode)
     ))

;;** folding
;;*** outline
(autoload 'outline-cycle  "outline-magic"
  "Visibility cycling for outline(-minor)-mode." t)
(autoload 'outline-move-subtree-up  "outline-magic"
  "Move the currrent subtree up past ARG headlines of the same level." t)
(autoload 'outline-move-subtree-down  "outline-magic"
  "Move the currrent subtree down past ARG headlines of the same level." t)


;;*** other folding
(autoload 'hide-region-hide  "hide-region"
  "Hides a region by making an invisible overlay over it and save the" t)
(autoload 'hide-region-unhide  "hide-region"
  "Unhide a region at a time, starting with the last one hidden and" t)


;;** rectangle
(autoload 'rectplus-copy-rectangle  "rect+"
  "Copy rectangle area." t)
(autoload 'rectplus-insert-number-rectangle  "rect+"
  "Insert incremental number into each left edges of rectangle's line." t)

(define-key ctl-x-r-map (kbd "M-w") 'rectplus-copy-rectangle)
(define-key ctl-x-r-map (kbd "M-n") 'rectplus-insert-number-rectangle)

;;** misc
(idle-require 'volatile-highlights)

(idle-require 'dired+)

(idle-require 'buff-menu+)
(eval-after-load "buff-menu+"
  `(progn
     (load-library "buff-menu")))

;;(idle-require 'vc+) ;;disabled as it's buggy

(idle-require 'menu-bar+)

(idle-require 'info+)

(idle-require 'mouse3)
