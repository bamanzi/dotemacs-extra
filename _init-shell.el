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

;; *** pcompletion
(eval-after-load "eshell"
  `(progn
     (if (executable-find "git")
         (require 'pcmpl-git nil t))
     (if (and (executable-find "hg")
              (require 'pcase nil t))
         (require 'pcmpl-args nil t))
     ))

;; **** pcomplete + auto-complete
;; http://emacswiki.org/emacs/EshellCompletion#toc4
(defun ac-pcomplete-candidates ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(eval-after-load "auto-complete"
  `(progn
     (setq ac-source-pcomplete
           '((candidates . ac-pcomplete-candidates)))
     (defun ac-complete-pcomplete ()
       (interactive)
       (auto-complete '(ac-source-pcomplete)))

     (add-to-list 'ac-modes 'eshell-mode)
     (add-to-list 'ac-modes 'shell-mode)
     ))

(define-key global-map (kbd "<apps> , p") 'ac-complete-pcomplete)

(eval-after-load "cheatsheet"
  `(progn
     (cheatsheet-add :group 'Eshell
                     :key "M-x ac-complete-pcomplete"
                     :description "complete shell commands & args with pcomplete + auto-complete.")
     (cheatsheet-add :group 'Shell-mode
                     :key "M-x ac-complete-pcomplete"
                     :description "complete shell commands & args with pcomplete + auto-complete.")
     t))


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

;; *** cheatsheet
(progn
  (cheatsheet-add :group 'Eshell :key "$ ?"        :description "esh-help-run-help (man)")
  (cheatsheet-add :group 'Eshell :key "$ help"     :description "esh-help-run-help (man)")
  (cheatsheet-add :group 'Eshell :key "$ j"        :description "list MRU paths (eshell-autojump)")
  (cheatsheet-add :group 'Eshell :key "$ j regexp" :description "jump to a MRU path (eshell-autojump)")
  t)


