;; ** backup-each-save (and keeping folder structure)
(idle-require 'backup-each-save)

(eval-after-load "backup-each-save"
  `(progn
     (add-hook 'after-save-hook 'backup-each-save)

     (when (memq system-type '(windows-nt ms-dos cygwin))

         (defun backup-each-save ()
           (let ((bfn (buffer-file-name)))
             (when (and (file-exists-p bfn)
                        (or backup-each-save-remote-files
                            (not (file-remote-p bfn)))
                        (funcall backup-each-save-filter-function bfn)
                        (or (not backup-each-save-size-limit)
                            (<= (buffer-size) backup-each-save-size-limit)))
               (copy-file bfn (backup-each-save-compute-location bfn) t t t))))


         ;; for windows, remove ':' in backup filename 
         (defun backup-each-save-compute-location (filename)
           ;;(let* ((containing-dir (file-name-directory filename))
           (let* ((containing-dir (replace-regexp-in-string ":" "" (file-name-directory filename)))
                  (basename (file-name-nondirectory filename))
                  (backup-container
                   (format "%s/%s"
                           backup-each-save-mirror-location
                           containing-dir)))
             (when (not (file-exists-p backup-container))
               (make-directory backup-container t))
             (format "%s/%s-%s" backup-container basename
                     (format-time-string backup-each-save-time-format))))
       )
     ))

;; ** vi emulation (viper)

;; *** viper/vimpulse addons
(eval-after-load "vimpulse"
  `(progn
     (require 'vimpulse-cjk nil t)
     (require 'vimpulse-textobj-between nil t)
     (require 'vimpulse-surround nil t)
     ))



;; ** log scratch contents
(idle-require 'scratch-log)


;; ** tempbuf: kill unused buffers in the background
(idle-require 'tempbuf)
(eval-after-load "tempbuf"
  `(progn
     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
     (if (require 'man nil t)
         (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode))
     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'diff-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'completion-list-mode 'turn-on-tempbf-mode)
     (add-hook 'occur-mode-hoo 'turn-on-tempbuf-mode)
     
     (if (require 'anything nil t)
         (add-hook 'anything-mode-hook 'turn-on-tempbuf-mode))
     
     ;;(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
     
     ))

;; ** outshine = outline + org-mode
;;TAB key for org-mode like folding

(eval-after-load "outshine"
  `(progn
     ;;(add-hook 'outline-minor-mode-hook 'outshine-hook-function)

     (set-face-attribute 'outshine-level-1 nil :inherit 'org-level-1)
     (set-face-attribute 'outshine-level-2 nil :inherit 'org-level-2)
     (set-face-attribute 'outshine-level-3 nil :inherit 'org-level-3)
     (set-face-attribute 'outshine-level-4 nil :inherit 'org-level-4)
     (set-face-attribute 'outshine-level-5 nil :inherit 'org-level-5)
     (set-face-attribute 'outshine-level-6 nil :inherit 'org-level-6)     
     ))

;; outorg is like "reverse Org-Babel":
;; call `outorg-edit-as-org' (C-z ') to edit outline(outshine) section in `org-mode',
;; and `outorg-copy-edits-and-exit' (M-#) to convert back.

(autoload 'outorg-edit-as-org "outorg"
  "Convert and copy to temporary Org buffer" t)

(eval-after-load "outorg"
  `(progn
     (load-library "outshine")
     ))

;; *** outline-org-like (my old package similar to `outshine')
(autoload 'outline-org-mode  "outline-org-like"
  "A special `outline-minor-mode' that use org-mode-style headings." t)
(autoload 'outline-org-heading-mode "outline-org-like"
  "eldoc" t)


;; ** nav
(autoload 'nav "nav"
  "Opens Nav in a new window to the left of the current one." t)

(autoload 'nav-toggle "nav"
  "Toggles the nav panel." t)
