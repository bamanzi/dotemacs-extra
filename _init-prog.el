;; ** simple-call-tree
(autoload 'sct-graphviz "sct-graphviz"
  "Analyze the simple tree call and display it as graphic." t)

(eval-after-load "sct-graphviz"
  `(when (eq system-type 'windows-nt)
     (setq sct-graphviz-dir temporary-file-directory)

     (setenv "PATH" (concat "e:\\tools\\graphviz;" (getenv "PATH")))
     (add-to-list 'exec-path "e:/tools/graphviz")
     ;;(executable-find "dot.exe")
     ))

