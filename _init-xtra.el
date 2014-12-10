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


;; ** realgud
;;https://github.com/rocky/emacs-dbgr

(add-to-list 'load-path "~/.emacs.d/packages/realgud")

(ignore-errors
  (idle-require 'realgud))

(autoload 'realgud-gdb    "realgud" nil t)

(autoload 'pdb            "realgud" nil t)
(autoload 'realgud-pdb    "realgud" nil t)
(autoload 'perldb         "realgud" nil t)
(autoload 'realgud-perldb "realgud" nil t)
(autoload 'rdebug         "realgud" nil t)
(autoload 'realgud-rdebug "realgud" nil t)

(autoload 'realgud-track-mode "realgud" nil t)





