;; ** shell enhancements
(eval-after-load "shell"
  `(progn
     (if (executable-find "git")
         (require 'pcmpl-git nil t))
     (if (and (executable-find "hg")
              (require 'pcase nil t))
         (require 'pcmpl-args nil t))
     ))


;; ** eshell enhancements
;; *** esh-help
;; https://github.com/tom-tan/esh-help/
;; `run-help' and eldoc support for eshell
(eval-after-load "eshell"
  `(when (require 'esh-help nil t)
     (setup-esh-help-eldoc)
     (defalias 'eshell/?    'esh-help-run-help)
     (defalias 'eshell/help 'esh-help-run-help)
     ))

;; *** autojump
(eval-after-load "eshell"
  '(require 'eshell-autojump nil t))
;;use command `j' to list your MRU path,
;;use command `j regexp' to jump to one

;; *** completion
(eval-after-load "eshell"
  `(progn
     (if (executable-find "git")
         (require 'pcmpl-git nil t))
     (if (and (executable-find "hg")
              (require 'pcase nil t))
         (require 'pcmpl-args nil t))
     ))

;; *** eshell-prompt-extras
;; https://github.com/hiddenlotus/eshell-prompt-extras
(eval-after-load 'esh-opt
  `(progn
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

;; *** eshell-did-you-mean
;; https://github.com/xuchunyang/eshell-did-you-mean
;; NOTE: this might make the response slow
(eval-after-load "eshell"
  `(progn
     ;;(eshell-did-you-mean-setup)
     ))

