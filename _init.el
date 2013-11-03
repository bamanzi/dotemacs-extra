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

;;*** god-mode
(autoload 'god-mode "god-mode"
  "Toggle global God mode." t)



;;** scratch-ext

(require 'scratch-ext nil t)

(setq inhibit-startup-screen nil)
(setq initial-major-mode 'org-mode)
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
         (insert-string "# C-x C-s : (scratch-save/bmz) -- save to daily log\n")
         (insert-string "# C-x C-v : (scratch-load/bmz) -- load from daily log\n")
         (insert-string "# C-c i   : (scratch-ext-insert-newest-log) -- load last log\n")
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


;;**  recent-jump
(setq rj-column-threshold 100)
(if (load "recent-jump" t)
    (recent-jump-mode t)
  (message "Warning: failed to load `recent-jump' (%s)." load-file-name))

(global-set-key (kbd "C-c <") 'recent-jump-backward)
(global-set-key (kbd "C-c >") 'recent-jump-forward)

;;** smart-mode-line
(idle-require 'smart-mode-line)
(eval-after-load "smart-mode-line"
  `(progn
     (sml/setup)
     (setq sml/hidden-modes '(" RJ"
                              " drag"
                              " back"
                              " org-link"
                              " Hi"
                              " -Chg"
                              " Undo-Tree"))
     ))


;;** hide some lines
(autoload 'hide-matching-lines "hide-lines"
  "Hide lines matching the specified regexp." t)
(autoload 'hide-non-matching-lines "hide-lines"
  "Hide lines that don't match the specified regexp." t)

;;** ibuffer-vc

(eval-after-load "ibuffer-vc"
  `(progn     
     (add-hook 'ibuffer-hook
               (lambda ()
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 (unless (eq ibuffer-sorting-mode 'alphabetic)
                   (ibuffer-do-sort-by-alphabetic))))

     (setq ibuffer-formats
           '((mark modified read-only vc-status-mini " "
                   (name 18 18 :left :elide)
                   " "
                   (size 9 -1 :right)
                   " "
                   (mode 16 16 :left :elide)
                   " "
                   (vc-status 16 16 :left)
                   " "
                   filename-and-process)))     
     ))


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


