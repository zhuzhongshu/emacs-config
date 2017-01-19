;;============================================================================================================
;;                                            elpy
;;============================================================================================================
(require 'elpy)
(elpy-enable);;对所有python文件都开启elpy模式，该函数自动将elpy-mode加入了python-mode-hook，还做了其他设置
(setq elpy-rpc-python-command
      (concat python-executable-path "python3"));;设置python后台程序为python3,python-executable-path在ein里面设置
(setq elpy-syntax-check-command
      (concat python-executable-path "flake8"));;设置python后台程序为flake8

(setq elpy-rpc-backend "rope")          ;设置rpc-backend为rope，也可以设置为jedi，提供代码解析功能。

(elpy-use-ipython
 (concat python-executable-path "ipython3"));;使用ipython3作为interactive shell


;;以下两行修正一个ipython3的问题，由于第一行的作用导致shell里的自动补全失效
;;第二行的目的是避免每次启动ipython时都有自动补全失效的提示
(setq python-shell-interpreter-args "--simple-prompt --pprint")
(add-to-list 'python-shell-completion-native-disabled-interpreters "ipython3")



(setq elpy-modules '(elpy-module-eldoc
                     elpy-module-company 
                     ;; elpy-module-flymake ;;语法检查用flycheck
                     elpy-module-yasnippet  ;;elpy自己提供了一些snippet
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation
                     elpy-module-sane-defaults))


(define-key elpy-mode-map (kbd "<C-return>") nil);;执行当前语句的命令跟标记冲突，重新绑定
(define-key elpy-mode-map (kbd "C-c C-f") nil);;elpy的键绑定跟smartparens冲突
(define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-current-statement);;执行当前语句
(define-key elpy-mode-map (kbd "<f10>")'elpy-format-code);;格式化代码，与clang-format一致


(provide 'init-elpy)
;;; init-elpy.el ends here
