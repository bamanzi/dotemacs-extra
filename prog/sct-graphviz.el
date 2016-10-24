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
    (let ((tmp-file (make-temp-file (expand-file-name ".tmp" sct-graphviz-dir) nil ".dot"))
          (viz-file (expand-file-name (concat (buffer-name (current-buffer)) ".jpg") sct-graphviz-dir)))
      (with-temp-file tmp-file
        (insert (sct-dot)))
      (cond
       ((null (executable-find graphviz-command))
        (error "Command `dot' not found. You need to install package `graphviz'."))
       (t
        (let ((cmd-return (shell-command-to-string (concat "dot " 
                                                           " -Tjpg " tmp-file
                                                           " -o " viz-file))))
          (if (not (zerop (length cmd-return)))
              (error "graphivz: error during external command: %s" cmd-return)
            (if (not (display-graphic-p))
                (progn
                  (message "`sct-graphviz' generated '%s' successfully, but it needs a graphical frame to display it." viz-file)
                  (find-file-other-window tmp-file))
              (let ((vct (get-buffer-create "*Visual Call Tree*")))
                (with-current-buffer vct
                  (image-toggle-display-text)
                  (erase-buffer)
                  (insert-file-contents viz-file)
                  (image-mode))
                (display-buffer vct))
              (delete-file tmp-file))))))))


;; make sure `anything-simple-call-tree' compatible with latest `simple-call-tree' (>= 20151116)
(eval-afer-load "anything-config"
  `(progn
     (defun anything-c-simple-call-tree-init-base (function message)
       (require 'simple-call-tree)
       (with-no-warnings
         (when (anything-current-buffer-is-modified)
           (anything-c-simple-call-tree-analyze-maybe)
           (let ((list (funcall function simple-call-tree-alist)))
             (with-current-buffer (anything-candidate-buffer 'local)
               (dolist (entry list)
                 (let ((funcs (concat "  " (mapconcat #'car (cdr entry) "\n  "))))
                   (insert (caar entry) message
                           (if (string= funcs "  ")
                               "  no functions."
                             funcs)
                           "\n\n"))))))))

     (defun anything-c-simple-call-tree-functions-callers-init ()
       (anything-c-simple-call-tree-init-base #'(lambda (sct-alist)
                                                  (simple-call-tree-invert))
                                              " is called by\n"))
     ))

(provide 'sct-graphviz)
;;; sct-graphviz ends here
