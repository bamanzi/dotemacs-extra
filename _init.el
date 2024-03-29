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

  (if (< emacs-major-version 24)
      (add-to-list 'load-path (concat dotemacs-extra-dir "_emacs23")))
  )

(require 'cheatsheet)
(global-set-key (kbd "<apps> <f1>") 'cheatsheet-show)

(defun try-require (feature)
  (require feature nil t))

(defalias 'try-idle-require 'try-require)

;; without `load-dotemacs-extra', your emacs would load fast, only subdirs added to `load-path'.
;; with `(load-dotemacs-extra 1)', some addon features would be loaded.
;; with `(load-dotemacs-extra 4)', more features would be loaded.

(defun load-dotemacs-extra (load-more)
  (interactive "p")

  (if (>= load-more 4)                    ; C-u C-u
      (progn
        ;; `try-idle-require' -> `idle-require' or `require'
        (if (fboundp 'idle-require-mode)
            (progn
              (defalias 'try-idle-require 'idle-require)
              (unless idle-require-mode
                (idle-require-mode 1)))
          (defalias 'try-idle-require 'try-require)))
    (defalias 'try-idle-require 'identity)) ;;to ignore `try-idle-require' in _init-*.el

  (mapc #'(lambda (file)
            (load-file file))
        (directory-files dotemacs-extra-dir 'full "^_init-.*.el$")))

