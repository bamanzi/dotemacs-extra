;;** viper 
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

;;*** Ex commands without entering viper-mode
;; stoem from http://www.advogato.org/person/chalst/diary/277.html
;;for ex commands supported by viper, refer `ex-token-alist'

(require 'viper-ex)
(require 'viper-keym)
(require 'viper-cmd)

(define-key global-map (kbd "ESC ESC :") 'viper-ex)


;;** scratch-ext

(require 'scratch-ext nil t)

(setq inhibit-startup-screen nil)
;;(setq initial-major-mode 'org-mode)
(run-with-timer 4 nil 'scratch-load/bmz)

(defun scratch-load/bmz ()
   "Load today's scratch content."
   (interactive)
   (let* ((scratch-ext-log-name-format "%Y/%m/%d.org")
          (buffer (get-buffer-create "*scratch*"))
          (file (expand-file-name (format-time-string scratch-ext-log-name-format (current-time))
                                   scratch-ext-log-directory)))
     (with-current-buffer buffer
       (if (file-readable-p file)
           (progn
             (erase-buffer)
             (insert-file-contents file)
             (funcall initial-major-mode))
         (message "Scratch log not found. %s" file)
         (end-of-buffer)
         (insert-string "(progn\n")
         (insert-string "  (local-set-key (kbd \"C-x C-s\") 'scratch-save/bmz) ;; save to daily log\n")
         (insert-string "  (local-set-key (kbd \"C-x C-v\") 'scratch-load/bmz) ;; load from daily log\n")
         (insert-string "  (local-set-key (kbd \"C-c i\")   'scratch-ext-insert-newest-log) ;; load last log\n")
         (insert-string "  )\n\n")
         )
       (local-set-key (kbd "C-x C-s") 'scratch-save/bmz)
       (local-set-key (kbd "C-x C-v") 'scratch-load/bmz)    
       (local-set-key (kbd "C-c i")   'scratch-ext-insert-newest-log))))

(defun scratch-save/bmz ()
  "Save scratch to daily log."
  (interactive)
  (require 'scratch-ext)
  ;;by default, save by timestamp
  (let ((scratch-ext-log-directory "~/.scratch.verbose/")
        (scratch-ext-log-name-format "%Y/%m/%d-%H%M%S"))
    (scratch-ext-save-scratch-to-file))
  
  ;;manually save to daily log
  (let ((scratch-ext-log-name-format "%Y/%m/%d.org"))
    (scratch-ext-save-scratch-to-file)))


;;*** set org-mode as major-mode of *scratch*
(if t
  (eval-after-load "scratch-ext"
    `(progn
       ;;redefine this function to add my own keybindings
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
