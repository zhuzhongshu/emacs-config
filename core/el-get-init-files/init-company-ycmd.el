(company-ycmd-setup)
(setq url-show-status nil);; 关掉minibuf里一直显示contacting host 127.0.0.1:xxxx的提示
(setq company-ycmd-enable-fuzzy-matching t);;开启fuzzy matching，可能会有点慢
;;默认值是2，输入两个字符就会自动启动ycmd补全，官方建议改的很大以防止这种情况的出现，同时不会影响输入::或->自动触发补全，
(setq ycmd-min-num-chars-for-completion 999)
(add-hook 'c-mode-common-hook
          (lambda ()
            (set (make-local-variable 'company-backends)  (quote (company-ycmd
                                                                  company-yasnippet
                                                                  company-files
                                                                  )))))

(add-hook 'LaTeX-mode-hook (lambda ()
                           (setq-local company-backends
                                       (append '(company-math-symbols-latex company-latex-commands)
                                               company-backends))))
;; 将下面的代码替换company-template中的同样定义来改变快捷键的绑定
(setq company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab] 'company-other-backend)
    (define-key keymap [(meta tab)] 'company-template-forward-field)
    (define-key keymap (kbd "M-TAB") 'company-template-forward-field)
    keymap))

(provide 'init-company-ycmd)
;;; init-company-ycmd.el ends here
