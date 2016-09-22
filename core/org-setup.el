;;============================================================================================================
;;                                            org-mode
;;============================================================================================================
(require 'org-install)
;; (require 'org-publish)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; not needed since Emacs 22.2
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords '((sequence "TODO" "DOING" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-log-done 'note);;TODO 设置成完成时，会提示输入批注
(setq org-startup-indented t)
(add-hook 'org-mode-hook 'auto-complete-mode)
(add-hook 'org-mode-hook (lambda ()
                           (company-mode -1)                           
                           (local-set-key [tab] 'org-cycle)
                           (local-set-key [(control return)] 'set-mark-command)
                           (setq truncate-lines nil)))
(provide 'org-setup)
;;; org-setup.el ends here
