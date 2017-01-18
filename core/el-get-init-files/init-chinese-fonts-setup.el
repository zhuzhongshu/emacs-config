(require 'chinese-fonts-setup)
;; 让 chinese-fonts-setup 随着 emacs 自动生效。

(chinese-fonts-setup-enable)
;; 让 spacemacs mode-line 中的 Unicode 图标正确显示。


(setq cfs-profiles
    '("program" "org-mode" "read-book"))

(setq cfs--profiles-steps '(("program" . 7)
                            ("org-mode" . 7)
                            ("read-book" . 7)))

(setq-default line-spacing 0.1)
;;下面是自动设置line-spacing的代码
;; (defvar my-line-spacing-alist
;;       '((9 . 0.1) (10 . 0.9) (11.5 . 0.2)
;;         (12.5 . 0.2) (14 . 0.2) (16 . 0.1)
;;         (18 . 0.2) (20 . 1.0) (22 . 0.2)
;;         (24 . 0.2) (26 . 0.2) (28 . 0.2)
;;         (30 . 0.2) (32 . 0.2)))

;; (defun my-line-spacing-setup (fontsizes-list)
;;   (let ((fontsize (car fontsizes-list))
;;         (line-spacing-alist (copy-list my-line-spacing-alist)))
;;     (dolist (list line-spacing-alist)
;;       (when (= fontsize (car list))
;;         (setq line-spacing-alist nil)
;;         (setq-default line-spacing (cdr list))))))

;; (add-hook 'cfs-set-font-finish-hook #'my-line-spacing-setup)



(provide 'chinese-fonts-setup)
;; ;;; init-auctex-latexmk.el ends here
