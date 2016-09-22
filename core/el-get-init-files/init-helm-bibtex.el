(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography '("/media/simplex/E/Doc/zotero/simplex.bib"))
(setq helm-bibtex-library-path '("/media/simplex/E/Doc/zotero/"))
(setq helm-bibtex-additional-search-fields '(journal))
(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (start-process "okular" "*helm-bibtex-okukar*" "/usr/bin/okular" fpath)))
;;自定义文章名称，作者，期刊等的颜色
(defface helm-bibtex-title-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The title face for `helm-bibtex' .")

(defface helm-bibtex-author-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The author face for `helm-bibtex' .")

(defface helm-bibtex-journal-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The jornal face for `helm-bibtex' .")
(defface helm-bibtex-year-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The year face for `helm-bibtex' .")

(defun helm-bibtex-candidates-formatter (candidates source)
 "自己改写的函数，替换了在helm窗口中显示的内容，增加了期刊名，去掉了附件信息并添加了色彩
  颜色使用上面定义的几个变量，把它替换helm-bibtex.el中的对应函数即可。"
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (helm-bibtex-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
   for fields = '("title" "year" "author" "journal"  )
   else
   for fields = '("title" "year" "editor"  "journal" )
   for fields = (-map (lambda (it)
                        (helm-bibtex-clean-string
                          (helm-bibtex-get-value it entry " ")))
                      fields)
   ;;处理作者项
   for fields = (-update-at 1 'helm-bibtex-shorten-authors fields)
   ;;分别给这几项字符串加上颜色
   for fields = (-update-at 0 (lambda (str) (propertize str 'face 'helm-bibtex-title-face)) fields)
   for fields = (-update-at 1 (lambda (str) (propertize str 'face 'helm-bibtex-year-face)) fields)
   for fields = (-update-at 2 (lambda (str) (propertize str 'face 'helm-bibtex-author-face)) fields)
   for fields = (-update-at 3 (lambda (str) (propertize str 'face 'helm-bibtex-journal-face)) fields)      
   collect
   ;;这个长度调整起来比较麻烦，不同显示器可能还得做不同的设置
   (cons (s-format " $0\t$1\tAuthor : $2\tJournal: $3" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s ))
                              fields (list (- width 20) 4 (- width 25) (- width 15) )))
         entry-key)))
(defun helm-bibtex-open-file-zzs (_)
  "自定义的打开附件函数.
zotero导出的bib有个file关键词,指出了附件的位置，可以根据这个来打开条目相应的附件.
目前支持pdf和html,因为基本只有这两种附件类型，"
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let* ((entry (helm-bibtex-get-entry key))
             (file-str (cdr (assoc "file" entry)))               
             (file-full-name (progn
                              (string-match ":\\([^:]*\\):" file-str)
                              ;;看要不要加上helm-bibtex-library-path
                              ;; (concat (car helm-bibtex-library-path)  (match-string 1 file-str))
                              (match-string 1 file-str)))
             (file-type (progn
                          ;;如果同时有pdf和html，则匹配第一个出现的文件类型
                          (string-match "\.\\([[:alpha:]]*\\)$" file-full-name)
                          (match-string 1 file-full-name))))
             (if (string= file-type "pdf")
                 (-each (list (concat "/media/simplex/E/Doc/zotero/" file-full-name)) helm-bibtex-pdf-open-function)
               (if (string= file-type "html")
                    (start-process "firefox" "*helm-bibtex-firefox*" "/usr/bin/firefox" (concat "/media/simplex/E/Doc/zotero/" file-full-name))
                                   (print file-type)))))))

(setq helm-source-bibtex
  '((name                                      . "BibTeX entries")
    (init                                      . helm-bibtex-init)
    (candidates                                . helm-bibtex-candidates)
    (filtered-candidate-transformer            . helm-bibtex-candidates-formatter)
    (action . (("Insert BibTeX key"            . helm-bibtex-insert-key)
               ("Open PDF or HTML attachment"  . helm-bibtex-open-file-zzs)
               ("Open PDF file (if present)"   . helm-bibtex-open-pdf)
               ("Open URL or DOI in browser"   . helm-bibtex-open-url-or-doi)
               ("Insert citation"              . helm-bibtex-insert-citation)
               ("Insert reference"             . helm-bibtex-insert-reference)
               ("Insert BibTeX entry"          . helm-bibtex-insert-bibtex)
               ("Attach PDF to email"          . helm-bibtex-add-PDF-attachment)
               ("Edit notes"                   . helm-bibtex-edit-notes)
               ("Show entry"                   . helm-bibtex-show-entry)))))

(provide 'init-helm-bibtex)
;;; init-helm-bibtex.el ends here
