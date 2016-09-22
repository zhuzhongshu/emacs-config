;;; package --- Summary
;; 自己写的一些小函数

;;; Commentary:

;;; code:

;;

(defun insert-time (format-string)
  "在当前位置插入当前时间，利用emacs自带的函数,格式由FORMAT-STRING提供."
  (interactive
   (list (read-string "format string: " nil nil "%Y-%m-%d %H:%M:%S")))
  (insert-before-markers  (format-time-string format-string)))

;;;; 另一个版本，用系统的date命令
;; (defun insertion-filter (proc time)
;;   (insert-before-markers time))
;; (defun insert-time-using-date (format-string)
;;   (interactive
;;    (list (read-string "format string: " nil nil "+%Y-%m-%d %H:%M:%S")))
;;   (set-process-filter (start-process "Date" nil "date" format-string) 'insertion-filter))

(defun convert-coding-system-and-save (coding-system-in coding-system-out)
  "将当前buffer用CODING-SYSTEM-IN解码,然后用CODING-SYSTEM-OUT编码保存.
函数先用某种编码格式来解码文件并显示,然后提示是否保存"
  (interactive
   (list (read-coding-system "Coding system for reading file (default gb18030)" 'gb18030)
         (read-coding-system "Coding system for saving file (default utf-8)" 'utf-8)))
  (let ((coding-system-for-read coding-system-in))
    (revert-buffer t t t))
  (if (y-or-n-p "Besure of saving file? ")
      (progn (set-buffer-file-coding-system coding-system-out)
            (save-buffer))))

(defun gb18030 ()
  "将当前buffer的编码直接改成gb18030(注:gb18030可以正确编码windows下建立的文档).
该函数不做任何请求确认提示,也不会保存更改,下次打开文件时仍然需要重新编码"
  (interactive)
  (let ((coding-system-for-read 'gb18030))
    (revert-buffer t t t)))

(defun save-as-utf8 ()
  "将当前buffer用gb18030重新编码并另存为utf-8格式.
该函数同样不做任何提示,而且这会改变文件本身."
  (interactive)
  (gb18030)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))


(defun zzs-octave ()
  "从当前buffer的右方开启一个新窗口,运行octave."
  (interactive)
  (let ((this-buffer (current-buffer))
        (this-window (selected-window)))
    (run-octave)
    (setq this-window (selected-window))
    (delete-other-windows)
    (split-window nil (floor (* (/ 2.0 5.0) (window-body-width))) 'left);;窗口是2:3来划分的
    (switch-to-buffer-other-window this-buffer)
    (select-window this-window)
    (window-configuration-to-register ?o)))
;; 这个版本在遇到重名的buffer时,会根据buffer-name的<>部分创立shell buffer,
;; 但是遇到不重名的buffer,则不会创建新的shell buffer
;; (defun shell-new ()
;;   (interactive)
;;   (let ((shell-new-buffer-name
;;          (concat "*shell*"
;;                  (progn (string-match "<?.*>?\\(<.*>\\)" (buffer-name))
;;                         (match-string 1 (buffer-name))))))
;;     (shell shell-new-buffer-name)))


(defun shell-new (bufname)
  "建立新的shell buffer，不过不如multi-term好用"
  (interactive
   (list
    (read-string "Customized buffer name: ")))
  (if (> (length bufname) 0)
      (shell (concat "*shell-" bufname "*"))
    (shell (concat "*shell->" (buffer-name) "*"))));;如果bufname已经存在，则自动在名称后面加上<number>用以区分
;;
(defun zzs-multi-term (bufname)
  "增加了重命名选项，关闭company-mode，代之以zsh或bash自带的补全"
  (interactive
   (list
    (read-string "Customized buffer name: ")))
  (multi-term)
  (if (> (length bufname) 0)
      (rename-buffer (concat "*term-" bufname "*") t));;如果bufname已经存在，则自动在名称后面加上<number>用以区分
  (company-mode -1))
(global-set-key [(control c) (control t)] 'zzs-multi-term)

;;
(defun zzs-shell ()
  "和zzs-octave一样."
  (interactive)
  (let ((this-buffer (current-buffer))
        (this-window (selected-window)))
    (shell-new)
    (setq this-window (selected-window))
    (split-window nil (floor (* (/ 2.0 5.0) (window-body-width))) 'left);;窗口是2:3来划分的
    (switch-to-buffer-other-window this-buffer)
    (select-window this-window)
    (window-configuration-to-register ?s)))

;;
(defun golden-at-point-run-search (search)
  "调用goldendict查询单词,借用了zeal-at-point中的代码, SEARCH 是要查找的单词."
  (if (executable-find "goldendict")
      (start-process "Golden" nil "goldendict" search)
    (message "GoldenDict wasn't found"))
  )
(defun golden-at-point (&optional edit-search)
  "Search for the word at point in Golden.  EDIT-SEARCH."
  (interactive "P")
  (let* ((thing (if mark-active
                    (buffer-substring (region-beginning) (region-end))
                  (thing-at-point 'symbol)))
         (search thing))
    (golden-at-point-run-search
     (if (or edit-search (null thing))
         (read-string "Golden search: " search)
       search))))
(global-set-key [(control c) (s)] 'golden-at-point)

;;
(defun iwb ()
  "缩进整个buffer,并将所有的tab键转换成空格."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;

;;设置eshell 中输入clear命令清屏
;; (defun eshell/clear ()
;;   "Clears the shell buffer ala Unix's clear or DOS' cls"
;;   (interactive)
;;   ;; the shell prompts are read-only, so clear that for the duration
;;   (let ((inhibit-read-only t))
;;     ;; simply delete the region
;;     (delete-region (point-min) (point-max))))

(defun clear-shell ()
  "\\C-\\l清屏函数."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'shell-mode-hook(lambda ()
                            (local-set-key (kbd "C-l") 'clear-shell)))
(add-hook 'inferior-octave-mode-hook(lambda ()
                                      (local-set-key (kbd "C-l") 'clear-shell)))
(add-hook 'comint-mode-hook(lambda ()
                             (local-set-key (kbd "C-l") 'clear-shell)))
;;multi-term中的键绑定需要用它提供的alist
(add-to-list 'term-bind-key-alist
             '("C-l" . clear-shell))
(multi-term-keystroke-setup)

(defun replace-eq-braket ()
  "将包围latex公式的$$换成\(\),用来匹配博客园写博客的要求。."
  (interactive)
  (while (re-search-forward "\\$\\([^\$]*\\)\\$" nil t)
    (replace-match "\\\\(\\1\\\\)" nil nil)))


(defun replace-month ()
  "Zotero导出的bib文件中，月份用中文表示，需要替换成对应的英文缩写。."
  (interactive)
  (setq month-name-alist '(
;;把十一月和十二月放在前面是为了防止字符串中包含的一月和二月被替换掉
                           ("十一月" . "Nov.")
                           ("十二月" . "Dec.")                           
                           ("一月" . "Jan.")
                           ("二月" . "Feb.")
                           ("三月" . "Mar.")
                           ("四月" . "Apr.")
                           ("五月" . "May")
                           ("六月" . "Jun.")
                           ("七月" . "Jul.")
                           ("八月" . "Aug.")
                           ("九月" . "Sep.")
                           ("十月" . "Oct.")))
  (dolist (month-name month-name-alist)
    (goto-char (point-min))
    (while (search-forward (car month-name) nil t)
      (replace-match (cdr month-name) t t))))
(defun delete-month ()
  (interactive)
  (setq month-name-alist '(
;;把十一月和十二月放在前面是为了防止字符串中包含的一月和二月被替换掉

                           (  "Nov."  .  " ")
                           (  "Dec."  .  " ")                           
                           (  "Jan."  .  " ")
                           (  "Feb."  .  " ")
                           (  "Mar."  .  " ")
                           (  "Apr."  .  " ")
                           (  "May"   .  " ")
                           (  "Jun."  .  " ")
                           (  "Jul."  .  " ")
                           (  "Aug."  .  " ")
                           (  "Sep."  .  " ")
                           (  "Oct."  .  " ")
                           (  "November"  .  " ")
                           (  "December"  .  " ")                           
                           (  "January"  .  " ")
                           (  "February"  .  " ")
                           (  "March"  .  " ")
                           (  "April"  .  " ")
                           (  "May"   .  " ")
                           (  "June"  .  " ")
                           (  "July"  .  " ")
                           (  "August"  .  " ")
                           (  "September"  .  " ")
                           (  "October"  .  " ")))
  (dolist (month-name month-name-alist)
    (goto-char (point-min))
    (while (search-forward (car month-name) nil t)
      (replace-match (cdr month-name) t t))))

(provide 'functions)
;;; functions.el ends here

  
