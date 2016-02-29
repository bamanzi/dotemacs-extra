;; ** desktop-registry
(autoload 'desktop-registry-change-desktop  "desktop-registry"
  "Change to the desktop named NAME." t)

(global-set-key (kbd "<f12> C-l") 'desktop-registry-change-desktop)

(idle-require 'desktop-registry)

(eval-after-load "desktop-registry"
  `(progn
     (desktop-registry-auto-register 1)
     ))

(unless (fboundp 'file-name-base)
  (defun file-name-base (&optional filename)
    "Return the base name of the FILENAME: no directory, no extension.
FILENAME defaults to `buffer-file-name'."
    (file-name-sans-extension
     (file-name-nondirectory (or filename (buffer-file-name))))))

(require 'cl-lib)
(unless (fboundp 'cl-find)
  (defalias 'cl-find 'find))


;; ** switch off/on touchpad when emacs gets/loses focus
;; https://www.reddit.com/r/emacs/comments/38o0tr/i_have_to_share_this_switch_your_touchpad_off/
(defvar touchpad-device-name nil
  "The name of your touchpad device.

You can figure it out using

xinput --list

e.g. for me it's \"SynPS/2 Synaptics TouchPad\"")

(defun turn-off-mouse (&optional frame)
  (interactive)
  (if touchpad-device-name
      (shell-command (format "xinput --disable \"%s\"" touchpad-device-name))
    (shell-command "synclient TouchpadOff=1")))

(defun turn-on-mouse (&optional frame)
  (interactive)
  (if touchpad-device-name
      (shell-command (format "xinput --enable \"%s\"" touchpad-device-name))
    (shell-command "synclient TouchpadOff=0")))

;; only Emacs >= 24.4 has `focus-in-hook' and `focus-out-hook'
(when (and (boundp 'focus-in-hook)
           (eq window-system 'x)
           (executable-find "synclient")
           (not (string-match "Couldn't find synaptics properties" (shell-command-to-string "synclient -l"))))
  (add-hook 'focus-in-hook #'turn-off-mouse)
  (add-hook 'focus-out-hook #'turn-on-mouse)
  (add-hook 'delete-frame-functions #'turn-on-mouse))

;; ** debugging
;; http://www.emacswiki.org/emacs/DebugMessages
;; Sometimes you want to find out where a particular error, warning or
;; just plain annoying message in Messages is coming from.

(when nil
  (defadvice message (before who-said-that)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))

  (defadvice error (before who-said-that)
    "Find out who said that thing. and say so."
    (let ((trace nil) (n 1) (frame nil))
      (while (setq frame (backtrace-frame n))
        (setq n     (1+ n) 
              trace (cons (cadr frame) trace)) )
      (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
      (ad-set-args 1 (cons trace (ad-get-args 1))) ))


  (defun who-said-that ()
    (interactive)
    (ad-enable-advice 'message 'before 'who-said-that)
    (ad-update 'message)

    (ad-enable-advice 'error 'before 'who-said-that)
    (ad-update 'error)
    )

  (defun who-said-that/disable ()
    (interactive)
    (ad-disable-advice 'message 'before 'who-said-that)
    (ad-update 'message)

    (ad-disable-advice 'error 'before 'who-said-that)
    (ad-update 'error)
    )  
)
