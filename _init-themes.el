;; emacs <= 23
(if (string< emacs-version "24")
    (progn
      (require 'color-theme nil t)
      (eval-after-load "color-theme"
        `(progn
           (if (require 'color-theme-tangotango nil t)
               (color-theme-tangotango)
             (if (require 'color-theme-molokai nil t)
                 (color-theme-molokai)))
           )))
  ;; emacs >= 24
  (progn
    (let ((theme-dir (locate-library "tangotango-theme")))
      (if theme-dir
          (add-to-list 'custom-theme-load-path (file-name-directory theme-dir))))

    (unless custom-enabled-themes
      (load-theme 'tango-dark)
      (custom-set-variables
       '(custom-enabled-themes (quote (tango-dark))))
      )))

