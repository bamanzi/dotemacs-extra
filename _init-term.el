;; ** menu bar on xterm
(autoload 'lacarte-execute-menu-command "lacarte"
  "Execute a menu-bar menu command in an alternative way." t)

(define-key global-map (kbd "ESC <f10>") 'lacarte-execute-menu-command)

(defun anything-lacarte ()
  (interactive)
  (anything  '(anything-c-source-lacarte)))

(define-key global-map (kbd "<f5> <f10>") 'anything-lacarte)
