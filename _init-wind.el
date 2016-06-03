;; * windows
;; ** move/swap
(idle-require 'bmz-window-misc)

(eval-after-load "bmz-window-misc"
  `(progn
     (global-set-key (kbd "<f11> g")     'ido-jump-to-window)
     
     (global-set-key (kbd "<f11> <M-left>")  'move-buffer-left)
     (global-set-key (kbd "<f11> <M-right>") 'move-buffer-right)
     (global-set-key (kbd "<f11> <M-up>")    'move-buffer-up)
     (global-set-key (kbd "<f11> <M-down>")  'move-buffer-right)

     (global-set-key (kbd "<f11> <M-S-left>")  'swap-buffer-left)
     (global-set-key (kbd "<f11> <M-S-right>") 'swap-buffer-right)
     (global-set-key (kbd "<f11> <M-S-up>")    'swap-buffer-up)
     (global-set-key (kbd "<f11> <M-S-down>")  'swap-buffer-right)

     (global-set-key (kbd "<f11> <M-backspace>") 'rotate-windows)
     (global-set-key (kbd "<f11> |")    'window-toggle-split-direction)
     
     ))

;; ** display buffer in other window/frame
;; e.g. `C-x 7 C-x b' works like `switch-to-buffer-other-window',
;;      `C-x 9 M-x find-function' works like `find-library-other-frame'

(autoload 'other-frame-window-mode "other-frame-window"
  "Minor mode for other frame/window buffer placement." t)

(unless (version< emacs-version "24.0")
  (idle-require 'other-frame-window))

(eval-after-load "other-frame-window"
  `(progn
     (other-frame-window-mode 1)
     ))

;; ** window jumping
(autoload 'window-numbering-mode "window-numbering"
  "A minor mode that assigns a number to each window." t)

(idle-require 'window-numbering)

(eval-after-load "window-numbering"
  `(progn     
     (window-numbering-mode 1)

     ;; Change the keybindings from `M-0'..`M-9' to `s-0'..`s-9'
     (dotimes (i 10)
       (eval `(progn
                (define-key window-numbering-keymap ,(read-kbd-macro (format "M-%d" i))
                  nil)
                (define-key window-numbering-keymap ,(read-kbd-macro (format "<f11> %d" i))
                  ,'(intern (format "select-window-%d" i))
                  )
                (define-key window-numbering-keymap ,(read-kbd-macro (format "s-%d" i))
                  ,'(intern (format "select-window-%d" i))
                  )
                )))
     (define-key window-numbering-keymap (kbd "s-0")     'select-minibuffer-window)
     (define-key window-numbering-keymap (kbd "<f11> 0") 'select-minibuffer-window)

     ;; make window number more clear on mode-line
     ;; TODO: make sure new frame get the face correctly copied
     (copy-face 'mode-line-buffer-id 'window-numbering-face)              
     (defun window-numbering-get-number-string (&optional window)
       (let ((s (concat " "
                        (int-to-string (window-numbering-get-number window))
                        "□ ")))
         (propertize s 'face 'window-numbering-face)))

     ))

;; *** with a fancy visual effect (similar to tmux)
(autoload 'switch-window "switch-window"
  "Display an overlay in each window showing a unique key, then" t)

(global-set-key (kbd "<f11> q") 'switch-window)
(global-set-key (kbd "<f11> g") 'switch-window)


;; ** other extensions
(idle-require 'window-extension)

(eval-after-load "window-extension"
  `(progn
     (global-set-key (kbd "<f11> <f11>") 'toggle-one-window)
     (global-set-key (kbd "<f11> V")     'delete-other-windows-vertically+)
     (global-set-key (kbd "<f11> H")     'delete-other-windows-horizontally+)
     
     (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
     (global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
     (global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
     ))


;; ** popwin
(defun popwin-mode ()
  (interactive)
  (if display-buffer-function
      ;; turn off
      (progn
        (setq display-buffer-function nil)
        (message "popwin deactivated."))
    (require 'popwin)
    (setq display-buffer-function 'popwin:display-buffer)
    (global-set-key (kbd "<f11> `") popwin:keymap)
    (message "popwin activated.")
    ))

(global-set-key (kbd "<f11> `") 'popwin-mode)


