
;; ** realgud
;;https://github.com/rocky/emacs-dbgr

(add-to-list 'load-path "~/.emacs.d/packages/realgud")

(ignore-errors
  (idle-require 'realgud))

(autoload 'realgud-gdb    "realgud" nil t)

(autoload 'pdb            "realgud" nil t)
(autoload 'realgud-pdb    "realgud" nil t)
(autoload 'perldb         "realgud" nil t)
(autoload 'realgud-perldb "realgud" nil t)
(autoload 'rdebug         "realgud" nil t)
(autoload 'realgud-rdebug "realgud" nil t)

(autoload 'realgud-track-mode "realgud" nil t)





