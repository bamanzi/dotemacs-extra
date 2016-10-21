;; -*- auto-byte-compile: t -*-
;; http://www.emacswiki.org/emacs/SimpleCallTree tips
;; 
;; maintainer: <hondana@gmx.com>
;;
;; Installation:
;;     (autoload 'sct-graphviz "simple-call-tree+" "Graphviz enhanced simple-call-tree" t)
;;
;; Note: automatically use ANYTHING-SIMPLE-CALL-TREE if GRAPHVIZ-COMMAND is not in EXEC-PATH
;;

(require 'simple-call-tree)
(require 'anything-config)

(defvar graphviz-command "dot"
  "Binary command used to generate graphs.")

(defvar sct-graphviz-dir (symbol-value 'temporary-file-directory)
  "Default temporary cache location.")

;;;###autoload
(defun simple-call-tree-list-functions-and-callers ()
  "List functions and callers in `simple-call-tree-alist'."
  (interactive)
  (let ((list (simple-call-tree-invert)))
    (switch-to-buffer (get-buffer-create "*simple-call-tree*"))
    (erase-buffer)
    (dolist (entry list)
      (let ((callee  (caar entry))
            (callers (mapconcat #'car (cdr entry) ", ")))
        (insert callee " is called by "
                (if (string= callers "")
                    "no functions."
                  callers)
                ".\n")))))

;;;###autoload
(defun simple-call-tree-list-callers-and-functions ()
  "List callers and functions in `simple-call-tree-alist'."
  (interactive)
  (let ((list simple-call-tree-alist))
    (switch-to-buffer (get-buffer-create "*simple-call-tree*"))
    (erase-buffer)
    (dolist (entry list)
      (let ((caller  (caar entry))
            (functions (mapconcat #'car
                                  (cdr entry)
                                  ", ")))
        (insert caller " calls "
                (if (string= functions "")
                    "no functions"
                  functions)
                ".\n")))))

(defun sct-dot ()
  "Generate dot file for graphviz from `simple-call-tree-alist'.

After calling `simple-call-tree-analyze', use `sct-dot' in an
empty buffer via `(insert (sct-dot))'.

Then save the file as \"my-file.dot\" and run
\"dot -Tjpg /path/to/my-file.dot -o result.jpg\" from command line."
  (concat "digraph G {\n" ;; default beginning of a dot file
          (mapconcat #'identity ;; end each line with a ";"
                     (mapcar #'(lambda (defun-list)
                                 "Called for each elemet (list) of `simple-call-tree-alist', create all the 'caller -> callee;' strings."
                                 (let* ((caller (car defun-list))
                                        (caller-name (substring-no-properties (car caller)))
                                        (callees (cdr defun-list)))
                                   (if (null callees)
                                       (concat "\"" caller-name "\"")
                                     (mapconcat #'(lambda (callee)
                                                    "Called with each callee, create 'caller -> callee' pairs."
                                                    (let ((callee-name (substring-no-properties (car callee))))
                                                      (concat "    \"" caller-name "\"" " -> " "\"" callee-name "\"")))
                                                callees
                                                ";\n"))))
                             simple-call-tree-alist)
                     ";\n")
          ";\n}"))

;;;###autoload
(defun sct-graphviz ()
  "Analyze the simple tree call and display it as graphic."
  (interactive)
  (simple-call-tree-analyze)
  (if (and (file-directory-p sct-graphviz-dir)
           (file-writable-p sct-graphviz-dir))
      ;; automatically purge temporary files on emacs killing
      (add-hook 'kill-emacs-hook
		#'(lambda ()
		    (mapc #'(lambda (file)
			      (when (file-writable-p file)
				(delete-file file)))
			  (directory-files sct-graphviz-dir t 
					   (format "\\.jpg\\'")))))
    (error (format "sct-graphviz: unable to access %s" sct-graphviz-dir)))
  (if (null (executable-find graphviz-command))
      (error "Command `dot' not found. You need to install package `graphviz'.")
    (let ((tmp-file (make-temp-file (expand-file-name ".tmp" sct-graphviz-dir) nil ".dot"))
          (viz-file (expand-file-name (concat (buffer-name (current-buffer)) ".jpg") sct-graphviz-dir)))
      (with-temp-file tmp-file
        (insert (sct-dot)))
      (let ((cmd-return (shell-command-to-string (concat "dot " 
                                                         " -Tjpg " tmp-file
                                                         " -o " viz-file))))
        (if (zerop (length cmd-return))
            (let ((vct (get-buffer-create "*Visual Call Tree*")))
              (with-current-buffer
                  vct
                (image-toggle-display-text)
                (erase-buffer)
                (insert-file-contents viz-file)
                (image-mode))
              (display-buffer vct))
          (error "graphivz: error during external command: %s" cmd-return)))
      (delete-file tmp-file))))

(provide 'sct-graphviz)
;;; sct-graphviz ends here
