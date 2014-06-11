;; * files & buffers
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


;; ** dired
(idle-require 'dired+)

(when (eq system-type 'windows-nt)
  (idle-require 'w32-browser))

;; *** dired-k
(autoload 'dired-k  "dired-k"
  "Highlighting dired buffer by file size, last modified time, and git status." t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map (kbd "K") 'dired-k)

     ;; always execute dired-k when dired buffer is opened
     ;; (add-hook 'dired-initial-position-hook 'dired-k)
     ))


;; ** nav (file browser)

(autoload 'nav "nav"
  "Opens Nav in a new window to the left of the current one." t)

(autoload 'nav-toggle "nav"
  "Toggles the nav panel." t)



;; ** vlf
(autoload 'vlf "vlf"  "View Large FILE." t)

(idle-require 'vlf)
