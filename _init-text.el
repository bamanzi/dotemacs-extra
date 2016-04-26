;; * org-mode
;; ** TOC

(eval-after-load "toc-org"
  `(progn
     (add-hook 'org-mode-hook 'toc-org-enable)
     ))

(defun org-insert-or-update-toc ()
  (interactive)
  (when (require 'toc-org nil t)
    (toc-org-enable)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        ;; find the first heading with the :TOC: tag
        (if (re-search-forward toc-org-toc-org-regexp (point-max) t)
            (toc-org-insert-toc)
          (message "You should add tag TOC to one of the heading lines."))))))

;; ** eldoc support
(eval-after-load "org"
  `(progn
     (if (and (require 'ob-core nil t)
              (require 'org-eldoc nil t))
         (message "Package `org-eldoc' loaded, you can use `eldoc-mode' in org buffers.")
       (message "WARN: package 'org-eldoc' requires org>8, but current version is %s. stop loading it." org-version))
     ))

;; ** attach images
;; insert image from local file or http
(autoload 'org-download-image "org-download"
  "Save image at address LINK to `org-download--dir'." t)

(defun org-insert-image-file (filename)
  (interactive "fImage file: ")
  (or (featurep 'org-download)
      (require 'org-download))
  (org-download-image filename))

;; some modifications over `org-download-screenshot' (ideas stolen from `org-screenshot')
;; + hide emacs frame unless C-u given
;; + we can give a base name for the file
;; + use `org-attach' to make storage path customizable for each document / heading
;; + use correct slash in filename when calling external screenshot command
(defun org-insert-screenshot (basename)
  "Capture screenshot and insert the resulting file.

The screen-shot tool is determined by `org-download-screenshot-method'."
  (interactive (list
                (read-string "sBase name for screenshot: " "screenshot")))
  (or (featurep 'org-download)
      (require 'org-download))  
  (let ((temp-file (concat (file-name-as-directory temporary-file-directory)
                           basename
                           ".png"))
        (attach-dir (org-download--dir))
        (frame (selected-frame)))
    (unless current-prefix-arg (iconify-frame frame))
    (shell-command (format org-download-screenshot-method
                           (convert-standard-filename temp-file)))
    (unless current-prefix-arg
      (make-frame-visible frame)
      (raise-frame frame))
    (org-download-image temp-file)))


(when (memq system-type '(ms-dos windows-nt))
    (setq org-download-screenshot-method "d:/wintools/IrfanView/i_view32.exe /capture=4 /convert=%s")
    (setq org-download-timestamp "_%Y-%m-%d_%H%M%S") ; ':' is invalid for filename os Windows   
    )

(eval-after-load "org-download"
  `(progn

     ;; use `org-attach' to manage attachment folders for current document / headings
     (defadvice org-download--dir (around use-org-attach activate)
       "Return the directory path for image storage with `org-attach' library.

This overrides original `org-download--dir' and makes `org-download-image-dir'
and `org-download-heading-lvl' obsolete."
       (require 'org-attach)
       (let ((result (or (org-attach-dir) ;; attach dir for current entry
                         (org-entry-get nil "ATTACH_DIR" t) ;; inherit from top level headings
                         (progn
                           (message "Hint: default attach folder used. Use `org-attach-set-directory' to change it.")
                           org-attach-directory))))
         (unless (file-exists-p result)
           (make-directory result t))
         (setq ad-return-value result)))


     (defadvice org-download-screenshot (around use-org-attach active)
       (let ((basename (read-string "sBase name for screenshot: " "screenshot-")))
         (org-insert-screenshot basename)))

     (defalias 'org-attach-screenshot 'org-download-screenshot)

     ))


;; ** export to asciidoc
(defun bmz/org-enable-exporting-to-asciidoc ()
  "Load package `ox-ascii' (org>=8) or `org-ascii' (org<8)."
  (require 'org)
  (if (string< org-version "8")
      ;; 1. export `org-export-as-ascii' to C-c C-e a/n/u  or C-c C-e A/N/U
      (if (require 'org-ascii nil t) ;;since org-6.33 core
          (message "Package `org-ascii' loaded. now you can publish org-mode to `asciidoc' with <C-c C-e a> or <C-c C-e A>"))

    ;; 2. org 8.x can export to ASCII directly
    (if (require 'ox-ascii nil t)  ;; expose ASCIIdocto C-c C-e menu
        (message "Package `ox-ascii' loaded. now you can publish org-mode to `asciidoc' with <C-c C-e t a> or <C-c C-e t A>"))
    ))

;; * asciidoc
(autoload 'adoc-mode "adoc-mode"
  "Major mode for editing AsciiDoc text files." t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))

;; * markdown
;; ** TOC
(autoload 'markdown-toc-generate-toc "markdown-toc"
  "Generate a TOC for markdown file at current point." t)
