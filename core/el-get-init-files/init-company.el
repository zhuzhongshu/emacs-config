;;============================================================================================================
;;                                            company-mode
;;============================================================================================================
;; (global-company-mode '(not term-mode))
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

(custom-set-variables
 '(company-require-match t)             ;设置为t时，如果输入错误补全窗口不会关闭，而是提示"matching input is required"
 '(company-auto-complete nil)
 '(company-auto-complete-chars (quote ignore))
 '(company-idle-delay 0)
 ;; 由于在C++中启用输入::,.和->自动补全需要启用idle-delay并且要在company-begin-commands里设置self-insert-command。
 ;; 但是这样就会导致输入指定个字符后自动开始补全，所以要把它设置的特别大......
 '(company-minimum-prefix-length 999)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 50)
 '(company-tooltip-offset-display (quote scrollbar))
 ;; '(company-transformers (quote (company-sort-by-occurrence)))
 ;; '(company-completion-started-hook (quote ((lambda (candidate)
 ;;                                             (company-filter-candidates)))))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend company-quickhelp-frontend)))

 '(company-backends (quote (company-capf
                            company-ispell
                            company-yasnippet
                            company-files
                            company-elisp
                            company-css
                            company-eclim
                            company-semantic
                            company-xcode
                            company-ropemacs
                            company-cmake
                            company-bbdb
                            (company-dabbrev-code company-gtags company-etags company-keywords)
                            company-oddmuse
                            company-dabbrev)))
 )
;; 定义tab键自动选择缩进/补全函数。注意补全函数不是采用company-complete系列而是采用company-other-backend，
;; 这样在当前后端没有合适的补全选项时会自动跳转到下一个后端。不会像company-complete系列命令那样强烈依赖于后端的排列顺序导致很多后端不能补全
(defun complete-and-filter ()
  (interactive)
  (company-other-backend)
  (company-filter-candidates));;启动company补全的时候自动启用过滤功能，可以部分的实现不分大小写，但也有很多弊端，暂未启用
;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-other-backend)
;;     (indent-according-to-mode)))
(defun indent-or-complete ()
  (interactive)
  (if (or (looking-at "\\_>") (looking-back "\\.\\|::"))
      (company-other-backend)
    (indent-according-to-mode)))

;; (defun complete-or-indent ()
;;   (interactive)
;;   (if (company-manual-begin)
;;       (company-other-backend)
;;     (indent-according-to-mode)))
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete-common-or-cycle)
  (define-key company-mode-map (kbd "<tab>") 'indent-or-complete)
  (dotimes (i 10)
    (define-key company-active-map (vector (+ (aref (kbd "C-0") 0) i))
      `(lambda () (interactive) (company-complete-number ,i))))
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)
  (define-key company-active-map (kbd "C-M-s") #'company-search-candidates)
  (define-key company-active-map (kbd "<tab>") #'company-other-backend)
  )
;;;;company-irony配置，记得要编译irony的server
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (setq irony-additional-clang-options (quote ("-std=c++11" "-I/usr/include/Eigen/")))
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)  (quote (company-irony
;;                                                                   company-yasnippet
;;                                                                   company-files)))))

(provide 'init-company)
;;; init-company.el ends here
