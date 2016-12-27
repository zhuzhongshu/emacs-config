;;============================================================================================================
;;                                             appearance
;;============================================================================================================
(require 'appearance-setup)

;;============================================================================================================
;;                                             =========
;;============================================================================================================
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (setq-default indent-tabs-mode  nil)    ;空格替换tab
  (setq backup-directory-alist '(("." . "~/.saves")));设置自动备份文件放在~/.saves目录下
  (setq auto-mode-alist
        (append '(("\\.\\(h\\|hpp\\|c\\)$" . c++-mode)) auto-mode-alist));;设置h文件和hpp文件用C++-mode，而不是默认的C-mode
  (when (fboundp 'winner-mode)
    (winner-mode)
    (windmove-default-keybindings))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



;;============================================================================================================
;;                                            global-variables
;;============================================================================================================
(setq my-home-dir (getenv "HOME"))


;;============================================================================================================
;;                                            spell-check
;;============================================================================================================
(setq-default ispell-program-name "aspell");;设置拼写检查的软件为aspell
(setq current-language-environment "en_US");;设置当前语言环境为en_US
(ispell-change-dictionary "en_US" t);;设置拼写检查的辞典为en_US:emacs24.3自动安装有ispell和辞典
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(global-set-key (kbd "<f7>") 'ispell-word)                      ;;F7键修改光标处的单词拼写,默认定义是M-$
(global-set-key (kbd "M-<f7>") 'flyspell-mode)          ;;开启/关闭flyspell mode
(global-set-key (kbd "C-<f7>") 'flyspell-buffer)
;;============================================================================================================
;;                                            electric-mode
;;============================================================================================================
(require 'electric)
(setq electric-indent-mode nil)                ;;开启缩进功能，用enter键也能取得C-j的效果
(setq electric-pair-mode nil);;系统本身内置的智能自动补全括号,用smartparens代替
;;============================================================================================================
;;                                            Tag Folding
;;============================================================================================================
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key [(control <)] 'hs-hide-block)                   ;;以下四个为代码折叠快捷键
(global-set-key [(control >)] 'hs-show-block)
(global-set-key [(control x) (control <)] 'hs-hide-all)
(global-set-key [(control x) (control >)] 'hs-show-all)
;;============================================================================================================
;;                                            hippie-expand
;;============================================================================================================

;; (setq hippie-expand-try-functions-list '(try-expand-whole-kill
;;                                          try-expand-line                    ; 补全当前行
;;                                          try-expand-dabbrev                 ; 搜索当前 buffer
;;                                          try-expand-dabbrev-visible         ; 搜索当前可见窗口
;;                                          try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
;;                                          try-expand-dabbrev-from-kill       ; 从 kill-ring 中搜索
;;                                          try-complete-file-name-partially   ; 文件名部分匹配
;;                                          try-complete-file-name             ; 文件名匹配
;;                                          try-expand-all-abbrevs             ; 匹配所有缩写词
;;                                          try-expand-list))
;;============================================================================================================
;;                                            clang-format
;;============================================================================================================
;; 这是从apper下载的，所以在site-lisp下，如果在MELPA下载，就不用这句命令了
;; 关于格式化选项，参照：http://clang.llvm.org/docs/ClangFormatStyleOptions.html
(add-to-list 'load-path "/usr/share/clang")
(load "clang-format.el")
;;设置F10和C-F10为局部快捷键，只对c-mode有效
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key [f10] 'clang-format-buffer)
            (local-set-key [C-f10] 'clang-format-region)))

;;============================================================================================================
;;                                              psql
;;============================================================================================================

;;用sql-connect命令打开一个新的sql进程并将当前文件连接到该进程，也可以直接利用函数psql-user-simplex
;;如果想让该文件绑定到现有sql进程，则使用sql-set-sqli-buffer命令
;;需要提前开启sql服务：
;; systemctl enable postgresql
;; systemctl start postgresql

;;配置预设sql-connect选项，可以免于在每次连接时输入账号密码信息
(setq sql-product 'postgres);;全局设置，比如在sql-mode里
(setq sql-connection-alist  
      '((user-simplex  
         (sql-product 'postgres)  
         (sql-server "localhost")  
         (sql-user "simplex")  
         ;; (sql-password "asdfasdf")  
         (sql-database "zzsweet")  
         (sql-port 5432)
         ))
      ;; '((pool-b  
      ;;    (sql-product 'mysql)  
      ;;    ;; (sql-server "mysql")  
      ;;    (sql-user "root")  
      ;;    (sql-password "asdfasdf")  
      ;;    ;; (sql-database "db1")  
      ;;    ;; (sql-port 6871)
      ;;    ))
      )  
      
(defun sql-connect-preset (name)  
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"  
  (eval `(let ,(cdr (assoc name sql-connection-alist))  
           (flet ((sql-get-login (&rest what)))  
             (sql-product-interactive sql-product)))))  
      
(defun psql-user-simplex ()
  "直接利用配置好的数据库连接设置`user-simplex'来连接数据库"  
  (interactive)  
  (sql-connect-preset 'user-simplex))  
(add-hook 'sql-mode-hook
          (lambda ()
            (local-set-key [(control x) (control e)] 'sql-send-region)
            (local-set-key [(control x) (control b)] 'sql-send-paragraph)
            (local-set-key [(control c) (control c)] 'sql-send-buffer)
            ))


;;============================================================================================================
;;                                              Tex
;;============================================================================================================
(require 'tex-setup)

;;============================================================================================================
;;                                              Math
;;============================================================================================================
(require 'math-setup)


;;============================================================================================================
;;                                              Org-mode
;;============================================================================================================
(require 'org-setup)



(provide 'basic)
;;; basic.el ends here
