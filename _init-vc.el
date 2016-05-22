
;; ** ibuffer-vc
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


;; ** highlighting  
;; *** git-gutter
;; https://github.com/syohex/emacs-git-gutter
(autoload 'git-gutter:toggle "git-gutter"
  "toggle to show diff information" t)

(defun _frame-reinit-git-gutter (&optional frame)
  "Choose `git-gutter' implementation from `git-gutter.el' or `git-gutter-fringe.el'."
  (interactive)
  (if (require 'git-gutter-fringe nil t)
      (if (display-graphic-p)
          (setq git-gutter:init-function 'git-gutter-fr:init
                git-gutter:view-diff-function 'git-gutter-fr:view-diff-infos
                git-gutter:clear-function 'git-gutter-fr:clear)
        (setq git-gutter:init-function nil
              git-gutter:view-diff-function 'git-gutter:view-diff-infos
              git-gutter:clear-function 'git-gutter:clear-diff-infos))))

;;(add-hook 'after-make-frame-functions '_frame-reinit-git-gutter)


;; *** git-gutter+
;; |                          | git-gutter+.el | git-gutter-fringe+.el |
;; | Works in tty frame       | +              | -                     |
;; | Works with linum-mode    | -              | +                     |
;; | Gutter on the right side | -              | +                     |

(autoload 'git-gutter+-mode "git-gutter+"
  "Git-Gutter mode" t)
(autoload 'global-git-gutter+-mode "git-gutter+"
  "Toggle Global-Git-Gutter+ mode on or off." t)

(autoload 'git-gutter+-toggle-fringe "git-gutter-fringe+"
  "Undocumented." t)


;; ** misc
;;(idle-require 'vc+) ;;disabled as it's buggy
