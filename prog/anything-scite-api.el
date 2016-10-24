;;; anything-scite-api.el --- browse SciTE api files with anything

;; Author: Ba Manzi <bamanzi@gmail.com>
;; URL: http://bitbucket.org/bamanzi/dotemacs-extra/src/default/prog/
;; Keyword: scite, api
;; Version: 0.1

(require 'auto-complete-scite-api) ;; for `ac-scite-api--read-api-files

(defvar anything-c-source-scite-api
  `((name . "Occur")
    (init . anything-c-scite-api--init)
    (candidates-in-buffer)
    (migemo)
    (get-line . anything-c-scite-api--get-line)
    (display-to-real . anything-c-display-to-real-line)
    (action . (("Insert line" . anything-c-scite-api--insert)
               ("Go to Line" . anything-c-action-line-goto)))
    (recenter)
    ;;(mode-line . anything-occur-mode-line)
    ;;(keymap . ,anything-occur-map)
    ;;(requires-pattern . 1)
    (delayed)))

(defun anything-c-scite-api--insert (lineno-and-content)
  (let* ((line (cadr lineno-and-content))
         (signature (replace-regexp-in-string ")\\( .*\\)" "" line nil nil 1)))
    (with-anything-current-buffer
      (insert-string signature))
    (message line)))

(defun anything-c-scite-api-get--line (s e)
  (format "%5d: %s" (line-number-at-pos (1- s)) (buffer-substring s e)))

(defun anything-c-scite-api--init ()
  (with-current-buffer (anything-candidate-buffer 'global)
    (erase-buffer)
    (mapcar #'(lambda (line)
                (insert-string line)
                (insert-string "\n"))
            (with-anything-current-buffer
              (ac-scite-api--read-api-files)))))

;;;###autoload
(defun anything-scite-api ()
  "Preconfigured Anything for SciTE API source."
  (interactive)
  (anything :sources 'anything-c-source-scite-api
            :buffer "*Anything SciTE API*"
            :input (thing-at-point 'symbol)
            ))

;;; anything-scite-api.el ends here
