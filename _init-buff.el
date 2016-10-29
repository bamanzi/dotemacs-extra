;; ** buffer-menu
(try-idle-require 'buff-menu+)
(eval-after-load "buff-menu+"
  `(progn
     (load-library "buff-menu")))


;; ** ibuffer
(autoload 'ibuffer-tramp-set-filter-groups-by-tramp-connection "ibuffer-tramp"
  "Set the current filter groups to filter by TRAMP connection." t)

(eval-after-load "ibuffer"
  `(progn
     (define-key ibuffer-mode-map (kbd "G t")   'ibuffer-tramp-set-filter-groups-by-tramp-connection)
     ))

(cheatsheet-add :group 'Ibuffer
                :key "G t"
                :description "M-x ibuffer-tramp-set-filter-groups-by-tramp-connection")

;; *** ibuffer-vc
;; make buffer list group by vc status
(autoload 'ibuffer-vc-set-filter-groups-by-vc-root "ibuffer-vc"
  "Set the current filter groups to filter by vc root dir." t)

(eval-after-load "ibuffer"
  `(progn
     (define-key ibuffer-mode-map (kbd "G v") 'ibuffer-vc-set-filter-groups-by-vc-root)
     ))

(eval-after-load "ibuffer-vc"
  `(progn
     ;; (add-hook 'ibuffer-hook
     ;;           (lambda ()
     ;;             (ibuffer-vc-set-filter-groups-by-vc-root)
     ;;             (unless (eq ibuffer-sorting-mode 'alphabetic)
     ;;               (ibuffer-do-sort-by-alphabetic))))

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

(cheatsheet-add :group 'Ibuffer
                :key "G v"
                :description "M-x ibuffer-vc-set-filter-groups-by-vc-root")


;; ** buffer sets

;; *** persp-mode
(autoload 'persp-mode  "persp-mode"
  "Toggle perspective mode." t)

;; *** frame-bufs
(autoload 'frame-bufs-mode "frame-bufs"
  "Toggle frame-bufs-mode on and off." t)


;; ** tempbuf: kill unused buffers in the background
(try-idle-require 'tempbuf)
(eval-after-load "tempbuf"
  `(progn
     (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
     (if (require 'man nil t)
         (add-hook 'Man-mode-hook 'turn-on-tempbuf-mode))
;;     (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'diff-mode-hook 'turn-on-tempbuf-mode)
     (add-hook 'completion-list-mode 'turn-on-tempbf-mode)
     (add-hook 'occur-mode-hoo 'turn-on-tempbuf-mode)
     
     (if (require 'anything nil t)
         (add-hook 'anything-mode-hook 'turn-on-tempbuf-mode))
     
     ;;(add-hook 'w3-mode-hook 'turn-on-tempbuf-mode)
     
     ))


;; ** scratch buffer for all major modes
(autoload 'scratch  "scratch"
  "Get a scratch buffer for the current mode." t)

(global-set-key (kbd "<f12> M-*") 'scratch)

(cheatsheet-add :group 'Misc
                :key "<f12> M-*"
                :description "scratch (Create (or switch to) scratch buffer for current major mode.")
