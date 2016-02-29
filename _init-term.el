;; ** menu bar on xterm
(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)

(defun anything-lacarte ()
  (interactive)
  (anything  '(anything-c-source-lacarte)))

(define-key global-map (kbd "<f5> <f10>") 'anything-lacarte)

;; ** tmux integration
(autoload 'emamux:send-command "emamux"
  "Send command to target-session of tmux" t)
(autoload 'emamux:run-command "emamux"
  "Run command" t)
(autoload 'emamux:send-region "emamux"
  "Send region to target-session of tmux" t) ;; emamux > 0.13 required

(setq emamux:completing-read-type 'ido)

(defun emamux:chdir-pwd-other-pane ()
  "Send 'cd `pwd`' to other pane."
  (interactive)
  (require 'emamux nil 'noerror)
  (emamux:check-tmux-running)
  (if (< (string-to-number (shell-command-to-string "tmux list-panes -t: | wc -l")) 2)
      (emamux:split-runner-pane))
  (let ((target ":.+") ;; the other pane
        (cmd (concat "cd " default-directory)))
    (emamux:reset-prompt target)
    (emamux:send-keys cmd target)
    ))
 
(define-key global-map (kbd "<f12> t") 'emamux:chdir-pwd-other-pane)
