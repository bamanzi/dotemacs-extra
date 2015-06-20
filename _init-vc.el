
;; ** ibuffer-vc
;; make buffer list group by vc status
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

;; ** git-gutter+
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
