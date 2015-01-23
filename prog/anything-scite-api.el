;;; anything-scite-api.el --- browse SciTE api files with anything

;; Author: Ba Manzi <bamanzi@gmail.com>
;; URL: http://bitbucket.org/bamanzi/dotemacs-extra/src/default/prog/
;; Keyword: scite, api
;; Version: 0.1

(require 'auto-complete-scite-api) ;; for `ac-scite-api--read-api-files

(defvar anything-c-source-scite-api
  `((name . "Occur")
    (init . anything-c-scite-api-init)
    (candidates-in-buffer)
    (migemo)
    (get-line . anything-c-occur-get-line)
    (display-to-real . anything-c-display-to-real-line)
    (action . (("Insert line" . anything-c-scite-api-insert)
               ("Go to Line" . anything-c-action-line-goto)))
    (recenter)
    ;;(mode-line . anything-occur-mode-line)
    ;;(keymap . ,anything-occur-map)
    ;;(requires-pattern . 1)
    (delayed)))

(defun anything-c-scite-api-insert (lineno-and-content)
      (insert-string (cadr lineno-and-content)))

(defun anything-c-scite-api-init ()
  (with-current-buffer (anything-candidate-buffer 'global)
    (erase-buffer)
    (mapcar #'(lambda (line)
                (insert-string line)
                (insert-string "\n"))
            (with-anything-current-buffer
              (ac-scite-api--read-api-files)))))

;;;###autoload
(defun anything-scite-api ()
  "Preconfigured Anything for SciTE API source.
If region is active, search only in region,
otherwise search in whole buffer."
  (interactive)
  (let ((anything-compile-source-functions
         ;; rule out anything-match-plugin because the input is one regexp.
         (delq 'anything-compile-source--match-plugin
               (copy-sequence anything-compile-source-functions))))
    (anything :sources 'anything-c-source-scite-api
              :buffer "*Anything SciTE API*"
              )))

;;; anything-scite-api.el ends here
