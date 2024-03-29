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
  (define-key global-map (kbd "M-l g g")  'google-translate-at-point)
  (define-key global-map (kbd "M-l M-g")  'google-translate-at-point)
  (define-key global-map (kbd "M-l G")    'google-translate-at-point)
  (define-key global-map (kbd "M-l g SPC")  'google-translate-query-translate)
  )

(progn
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l g g"    :description "google-translate-at-point")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l G"      :description "google-translate-at-point")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l g SPC"  :description "google-translate-query-translate")
  t)

;; ** dictionary
;; *** youdao dictionary
(autoload 'youdao-dictionary-search-at-point+  "youdao-dictionary"
  "Search word at point and display result with popup-tip." t)

(autoload 'youdao-dictionary-search-from-input "youdao-dictionary"
  "Search word from input and display result with buffer." t)

(progn
  (define-key global-map (kbd "M-l y y")   'youdao-dictionary-search-at-point+)
  (define-key global-map (kbd "M-l M-y")   'youdao-dictionary-search-at-point+)  
  (define-key global-map (kbd "M-l Y")     'youdao-dictionary-search-at-point+)
  (define-key global-map (kbd "M-l y SPC") 'youdao-dictionary-search-from-input)
  )


(progn
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l y y"    :description "youdao-dictionary-search-at-point+")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l Y"      :description "youdao-dictionary-search-at-point+")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l y SPC"  :description "youdao-dictionary-search-from-input")
  t)

;; *** dict protocol
(setq dictem-server "localhost")
(autoload 'dictem-run-search  "dictem" nil t)
(autoload 'dictem-run-match   "dictem" nil t)
(autoload 'dictem-run-define  "dictem" nil t)

(progn
  (define-key global-map (kbd "M-l d d") 'dictem-run-define)
  (define-key global-map (kbd "M-l M-d") 'dictem-run-define)
  (define-key global-map (kbd "M-l D")   'dictem-run-define)
  (define-key global-map (kbd "M-l d s") 'dictem-run-search)
  (define-key global-map (kbd "M-l d SPC") 'dictem-run-search)
  (define-key global-map (kbd "M-l d m") 'dictem-run-match)
  )

(progn
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l d d"    :description "dictem-run-define")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l D"      :description "dictem-run-define")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l d SPC"  :description "dictem-run-search")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l d s"    :description "dictem-run-search")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l d m"    :description "dictem-run-match")
  t)


(eval-after-load "dictem"
  `(progn
     (dictem-initialize)
     ))

;; *** sdcv
(autoload 'sdcv-search-input "sdcv"
  "Search WORD through the `command-line' tool sdcv." t)
(define-key global-map (kbd "M-l s SPC")  'sdcv-search-input)

;;(setq sdcv-dictionary-simple-list '("XDICT英汉辞典" "XDICT汉英辞典"))
(autoload 'sdcv-search-pointer+ "sdcv"
  "Translate current point word with command-line tool `sdcv'." t)
(define-key global-map (kbd "M-l s s")  'sdcv-search-pointer+)
(define-key global-map (kbd "M-l S")    'sdcv-search-pointer+)
(define-key global-map (kbd "M-l M-s")  'sdcv-search-pointer+)

(progn
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l s s"    :description "sdcv-search-pointer+")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l S"      :description "sdcv-search-pointer+")
  (cheatsheet-add :group 'Language-Tools/Dict :key "M-l s SPC"  :description "sdcv-search-input")
  t)


(defun sdcv-search-word-at-pt-mouse (event)
  (interactive "e")
  (mouse-set-point event)
  (require 'sdcv)
  (call-interactively 'sdcv-search-pointer+))

;;(global-set-key (kbd "<C-down-mouse-1>") 'sdcv-search-word-at-pt-mouse)


;; ** input methods
;; *** eim
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

;; *** fcitx.el (make fcitx work better in emacs)
(autoload 'fcitx-default-setup "fcitx"
  "Default setup for `fcitx'." t)

(when (and (eq window-system 'x)
           (> (length (shell-command-to-string "pidof fcitx")) 0))
  (try-idle-require 'fcitx))

(eval-after-load "fcitx"
  `(progn
     (fcitx-default-setup)
     
     (fcitx-prefix-keys-add "M-s")
     ))

;; ** misc
;; *** helm-unicode
(autoload 'helm-unicode "helm-unicode"
  "Precofigured ‘helm’ for looking up unicode characters by name." t)

(global-set-key (kbd "M-o *") 'helm-unicode)

(cheatsheet-add :group 'Language-Tools :key "M-x helm-unicode"
                :description "Look up unicode charactors by name.")


;; *** pinyin-search
(autoload 'isearch-forward-pinyin "pinyin-search"
  "Search Chinese forward by Pinyin." t)
(autoload 'isearch-backward-pinyin "pinyin-search"
  "Search Chinese backward by Pinyin." t)
(autoload 'pinyin-search "pinyin-search"
  "Search Chinese forward by Pinyin." t)
(autoload 'pinyin-search-backward "pinyin-search"
  "Search Chinese backward by Pinyin." t)

(progn
  (cheatsheet-add :group 'Language-Tools :key "M-x isearch-forward-pinyin"  :description "Search Chinese forward by Pinyin.")
  (cheatsheet-add :group 'Search         :key "M-x isearch-forward-pinyin"  :description "Search Chinese forward by Pinyin.")
  )
