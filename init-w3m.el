;; ** w3m

;;a recommended to download latest snapshot:
;;http://packages.debian.org/sid/w3m-el-snapshot

(ignore-errors
  (idle-require 'w3m-load))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;(setq browse-url-browser-function 'w3m-browse-url)

 (with-eval-after-load "w3m"
      (setq w3m-use-title-buffer-name nil)
      ;;(setq browse-url-browser-function 'w3m-browse-url)
	 
      (define-key w3m-mode-map (kbd "<down>") 'next-line)
      (define-key w3m-mode-map (kbd "<up>") 'previous-line)
      (define-key w3m-mode-map (kbd "<left>") 'backward-char)
      (define-key w3m-mode-map (kbd "<right>") 'forward-char)

      (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
      (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
      (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
      (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-next-page)       
    )

;; default to new tab
(defun w3m-new-tab ()
  (require 'w3m)
  (interactive)
  (w3m-copy-buffer nil nil nil t))

 (defun w3m-browse-url-new-tab (url &optional new-session)
   (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
   (require 'w3m)
   (w3m-new-tab)
   (w3m-browse-url url new-session))
;;(setq browse-url-browser-function 'w3m-browse-url-new-tab)
