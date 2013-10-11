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
