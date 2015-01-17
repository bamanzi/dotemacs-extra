;; ** w3m

;;a recommended to download latest snapshot:
;;http://packages.debian.org/sid/w3m-el-snapshot

(ignore-errors
  (idle-require 'w3m-load))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;;(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-default-display-inline-images t)
(setq w3m-use-title-buffer-name nil)

(eval-after-load "w3m"
  `(progn
      (define-key w3m-mode-map (kbd "<down>") 'next-line)
      (define-key w3m-mode-map (kbd "<up>") 'previous-line)
      (define-key w3m-mode-map (kbd "<left>") 'backward-char)
      (define-key w3m-mode-map (kbd "<right>") 'forward-char)

      (define-key w3m-mode-map (kbd "<C-down>") 'w3m-next-anchor)
      (define-key w3m-mode-map (kbd "<C-up>") 'w3m-previous-anchor)
      (define-key w3m-mode-map (kbd "<C-left>") 'w3m-view-previous-page)
      (define-key w3m-mode-map (kbd "<C-right>") 'w3m-view-next-page)

      (if (require 'w3m-lnum nil t)
          ;; use 'F' key to show link hint (F is bound to `w3m-lnum-goto')
          (add-hook 'w3m-mode-hook 'w3m-lnum-mode))
  
      (when (and (memq system-type '(windows-nt cygwin))
                 (not (getenv "LANG")))
        ;; w3m executable would crash if env var LANG not set
        ;; http://cygwin.com/ml/cygwin/2011-01/msg00069.html
        (setenv "LANG" "zh_CN"))

      ;; (setq w3m-command "e:/cygwin2/bin/w3m.exe")
      (when (and (eq system-type 'windows-nt)
                 (not (file-executable-p w3m-command)))
        (if (and (boundp 'cygwin-mount-cygwin-bin-directory) cygwin-mount-cygwin-bin-directory)
            (setq w3m-command (concat cygwin-mount-cygwin-bin-directory "/w3m.exe"))
          (if (executable-find "w3m.exe")
              (message "Could not find `w3m.exe'. You need to customize `w3m-command' to make `w3m-browser-url' work."))))
      ))

;; ** new tab
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

(defun w3m-find-file-new-tab (file)
  (interactive "fFilename: ")
  (require 'browse-url)  
  (w3m-new-tab)
  (w3m-find-file file))

(defun ffap-w3m-new-tab (url &optional new-session)
  "Browse url in w3m.
  If current frame has only one window, create a new window and browse the webpage"
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "Emacs-w3m URL: ")))
  (let ((w3m-pop-up-windows t)) ;;FIXME: necessary?
    (w3m-browse-url-new-tab url new-session)))


;; ** other window
(defun w3m-browse-url-other-window (url &optional new-session)
   (interactive (progn
		 (require 'browse-url)
		 (browse-url-interactive-arg "Emacs-w3m URL: ")))
   (require 'w3m)
   (let ((w3m-pop-up-windows t))
     (if (one-window-p) (split-window))
     (other-window 1)
     (w3m-browse-url url new-session)))
;;(setq browse-url-browser-function 'w3m-browse-url-other-window)

(defun w3m-find-file-other-window (file)
  (interactive "fFilename: ")
  (require 'browse-url)
   (let ((w3m-pop-up-windows t))
     (if (one-window-p) (split-window))
     (other-window 1)
     (w3m-find-file file)))

;; stolen from http://www.emacswiki.org/emacs/emacs-w3m#toc24
(defun ffap-w3m-other-window (url &optional new-session)
  "Browse url in w3m.
  If current frame has only one window, create a new window and browse the webpage"
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "Emacs-w3m URL: ")))
  (let ((w3m-pop-up-windows t))
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-browse-url url new-session)))

