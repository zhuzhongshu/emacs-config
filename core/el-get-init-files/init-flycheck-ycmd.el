(flycheck-ycmd-setup)
(add-to-list 'flycheck-disabled-checkers 'c/c++-clang);;禁用自带的clang checker（慢）
(add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
(add-hook 'python-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
;;用pop-up菜单显示出错信息,需要安装插件flycheck-pos-tip，该函数调用了flycheck中提供的flycheck-display-errors-function
;;变量来指定显示出错信息所调用的函数

(provide 'init-flycheck-ycmd)
;;; init-flycheck-ycmd.el ends here
