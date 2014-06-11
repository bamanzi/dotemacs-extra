;; ** windows
(idle-require 'bmz-window-misc)

(eval-after-load "bmz-window-misc"
  `(progn
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
