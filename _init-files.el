;; * files & buffers
;; ** backup-each-save (and keeping folder structure)
(try-idle-require 'backup-each-save)

(eval-after-load "backup-each-save"
  `(progn
     (add-hook 'after-save-hook 'backup-each-save)

     ;; make `backup-each-save' work for files on tramp/remote or in archive
     (defun backup-each-save ()
       (let* ((bfn (buffer-file-name))
              (backup-filename (backup-each-save-compute-location bfn)))
         (when (and (or backup-each-save-remote-files
                        (not (file-remote-p bfn)))
                    (funcall backup-each-save-filter-function bfn)
                    (or (not backup-each-save-size-limit)
                        (<= (buffer-size) backup-each-save-size-limit)))
           (if (file-exists-p bfn)
               (copy-file bfn backup-filename t t t)
             ;; file lives on tramp or in archive
             (let ((buffer-file-name backup-filename))
               (basic-save-buffer-1))))))

     ;; for windows, remove ':' in backup filename 
     (defun backup-each-save-compute-location (filename)
       ;;(let* ((containing-dir (file-name-directory filename))
       (let* ((filename-norm (replace-regexp-in-string ":" "/" filename))
              (containing-dir (file-name-directory filename-norm)) ;;modified: 
              (basename (file-name-nondirectory filename-norm))
              (backup-container
               (format "%s/%s"
                       backup-each-save-mirror-location
                       containing-dir)))
         (when (not (file-exists-p backup-container))
           (make-directory backup-container t))
         (format "%s/%s-%s" backup-container basename
                 (format-time-string backup-each-save-time-format))))
     
     ))



;; ** dired
(try-require 'dired+)

;; *** win32
(when (eq system-type 'windows-nt)
  (try-idle-require 'w32-browser))

(progn
  (cheatsheet-add :group 'Dired
                  :key "M-x dired-w32-browser"
                  :description "Run default Windows application associated with current line's file.")
  (cheatsheet-add :group 'Dired
                  :key "M-x dired-w32explore"
                  :description "Open Windows Explorer to current file or folder.")  
  t
  )

;; *** dired-k
(autoload 'dired-k  "dired-k"
  "Highlighting dired buffer by file size, last modified time, and git status." t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map (kbd "K") 'dired-k)

     ;; always execute dired-k when dired buffer is opened
     ;; (add-hook 'dired-initial-position-hook 'dired-k)
     ))

(progn
  (cheatsheet-add :group 'Dired
                  :key "K"
                  :description "dired-k (Highlighting dired buffer by file size, last modified time, and git status).")
  t
  )

;; *** dired-narrow
(autoload 'dired-narrow "dired-narrow"
  "Narrow a dired buffer to the files matching a string." t)
(autoload 'dired-narrow-regexp "dired-narrow"
  "Narrow a dired buffer to the files matching a regular expression." t)

(eval-after-load "dired"
  `(progn
     (define-key dired-mode-map (kbd "M-q") 'dired-narrow-regexp)
     ))

(progn
  (cheatsheet-add :group 'Dired
                  :key "M-q"
                  :description "dired-narrow")
  t
  )

;; ** nav (file browser)

(autoload 'nav "nav"
  "Opens Nav in a new window to the left of the current one." t)

(autoload 'nav-toggle "nav"
  "Toggles the nav panel." t)


;; ** vlf
(autoload 'vlf "vlf"  "View Large FILE." t)

(cheatsheet-add :group 'Open/Save
                :key "M-x vlf"
                :description "View large file")


;; ** open-junk-file
(autoload 'open-junk-file "open-junk-file"
  "Open a new file whose filename is derived from current time." t)

(cheatsheet-add :group 'Open/Save
                :key "M-x open-junk-file"
                :description "Open a new file whose filename is derived from current time.")

