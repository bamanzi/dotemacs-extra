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
  (if (< emacs-major-version 24)
      (add-to-list 'load-path (concat dotemacs-extra-dir "_emacs23")))
  )


(if (and (fboundp 'idle-require-mode) idle-require-mode) ;;`idle-require-mode' not finished yet
    (defalias 'idle-require 'require))


(mapc #'(lambda (file)
	  (load-file file))
      (directory-files dotemacs-extra-dir 'full "^_init-.*.el$"))

