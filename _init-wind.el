;; * windows
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

(idle-require 'window-extension)

(eval-after-load "window-extension"
  `(progn
     (global-set-key (kbd "<f11> <f11>") 'toggle-one-window)
     (global-set-key (kbd "<f11> d v")   'delete-other-windows-vertically+)
     (global-set-key (kbd "<f11> d h")   'delete-other-windows-horizontally+)
     
     (global-set-key [(control x) (?0)] 'sticky-window-delete-window)
     (global-set-key [(control x) (?1)] 'sticky-window-delete-other-windows)
     (global-set-key [(control x) (?9)] 'sticky-window-keep-window-visible)
     ))

;; ** window jumping
(autoload 'window-numbering-mode "window-numbering"
  "A minor mode that assigns a number to each window." t)

(idle-require 'window-numbering)

(eval-after-load "window-numbering"
  `(progn     
     (window-numbering-mode 1)

     ;; make window number more clear on mode-line
     ;; TODO: make sure new frame get the face correctly copied
     (copy-face 'mode-line-buffer-id 'window-numbering-face)              
     (defun window-numbering-get-number-string (&optional window)
       (let ((s (concat " "
                        (int-to-string (window-numbering-get-number window))
                        "□ ")))
         (propertize s 'face 'window-numbering-face)))

     (define-key window-numbering-keymap (kbd "s-0") 'select-minibuffer-window)
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


;; ** perspectives

;; *** persp-mode
(autoload 'persp-mode  "persp-mode"
  "Toggle perspective mode." t)

;; *** frame-bufs
(autoload 'frame-bufs-mode "frame-bufs"
  "Toggle frame-bufs-mode on and off." t)





