;; ** translate

;; *** google-translate
(autoload 'google-translate-at-point "google-translate"
  "Translate the word at point or the words in the active region." t)
(autoload 'google-translate-query-translate "google-translate"
  "Interactively translate text with Google Translate." t)

(defalias 'gtap 'google-translate-at-point)
(defalias 'gtqt 'google-translate-query-translate)

(setq google-translate-enable-ido-completion t
      google-translate-default-source-language "en"
      google-translate-default-target-language "zh-CN")

(progn
  (global-set-key (kbd "<apps> g g")   'google-translate-at-point)
  (global-set-key (kbd "<apps> G")     'google-translate-at-point)
  (global-set-key (kbd "<apps> g SPC") 'google-translate-query-translate)
  )

;; ** dictionary
;; *** youdao dictionary
(autoload 'youdao-dictionary-search-at-point+  "youdao-dictionary"
  "Search word at point and display result with popup-tip." t)

(autoload 'youdao-dictionary-search-from-input "youdao-dictionary"
  "Search word from input and display result with buffer." t)

(progn
  (global-set-key (kbd "<apps> y y")   'youdao-dictionary-search-at-point+)
  (global-set-key (kbd "<apps> Y")     'youdao-dictionary-search-at-point+)
  (global-set-key (kbd "<apps> y SPC") 'youdao-dictionary-search-from-input)
  )


;; *** dict protocol
(setq dictem-server "localhost")
(autoload 'dictem-run-search  "dictem" nil t)
(autoload 'dictem-run-match   "dictem" nil t)
(autoload 'dictem-run-define  "dictem" nil t)

(progn
  (global-set-key (kbd "<apps> d s") 'dictem-run-search)
  (global-set-key (kbd "<apps> d SPC") 'dictem-run-search)
  (global-set-key (kbd "<apps> d m") 'dictem-run-match)
  (global-set-key (kbd "<apps> d d") 'dictem-run-define)
  (global-set-key (kbd "<apps> D")   'dictem-run-define)
  )


(eval-after-load "dictem"
  `(progn
     (dictem-initialize)
     ))

;; *** sdcv
(autoload 'sdcv-search-input "sdcv"
  "Search WORD through the `command-line' tool sdcv." t)
(global-set-key (kbd "<apps> s SPC")  'sdcv-search-input)

;;(setq sdcv-dictionary-simple-list '("XDICT英汉辞典" "XDICT汉英辞典"))
(autoload 'sdcv-search-pointer+ "sdcv"
  "Translate current point word with command-line tool `sdcv'." t)
(global-set-key (kbd "<apps> s s")  'sdcv-search-pointer+)
(global-set-key (kbd "<apps> S")    'sdcv-search-pointer+)

(defun sdcv-search-word-at-pt-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (require 'sdcv)
  (call-interactively 'sdcv-search-pointer+))

;;(global-set-key (kbd "<C-down-mouse-1>") 'sdcv-search-word-at-pt-mouse)


;; * input methods
;; ** eim
;;https://github.com/wenbinye/emacs-eim

(add-to-list 'load-path "~/.emacs.d/packages/eim")

(autoload 'eim-use-package "eim" "Another emacs input method")
;; Tooltip 暂时还不好用
(setq eim-use-tooltip nil)

(register-input-method
 "eim-wb" "euc-cn" 'eim-use-package
 "五笔" "汉字五笔输入法" "wb.txt")
(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

(eval-after-load "eim"
  `(progn
     ;; 用 ; 暂时输入英文
     (require 'eim-extra)
     (global-set-key ";" 'eim-insert-ascii)
     ))

;; ** fcitx.el (make fcitx work better in emacs)
(autoload 'fcitx-default-setup "fcitx"
  "Default setup for `fcitx'." t)

(when (and (eq window-system 'x)
           (shell-command-to-string "pidof fcitx"))
  (idle-require 'fcitx))

(eval-after-load "fcitx"
  `(progn
     (fcitx-default-setup)
     
     (fcitx-prefix-keys-add "M-s")
     ))
