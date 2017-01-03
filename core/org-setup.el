;;============================================================================================================
;;                                            org-mode
;;============================================================================================================
(require 'org-install)
;; (require 'org-publish)
(require 'org-bullets)


(setq org-src-fontify-natively t)       ;org显示源码时用对应代码默认的高亮
(setq org-directory "/home/simplex/zzsweet/src/org") ;org-mode 默认目录
(setq org-default-notes-file (concat org-directory "/capture/scratch.org"))



(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) ; not needed since Emacs 22.2
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;配置org capture

;;设置捕捉模板
(setq org-capture-templates '
      (("s" "scratch " entry (file+datetree org-default-notes-file)
        "* %?\n创建于： %U\n  %i\n  %a")
       ("n" "note" entry (file+headline (concat org-directory "/capture/notes.org") "笔记")
        "* %?\n创建于： %U\n  %i\n  %a")
       ("j" "jobs" entry (file+datetree (concat org-directory "/capture/jobs.org"))
        "* %?\n创建于： %U\n  %i\n  %a")
       ("t" "TODO" entry (file+datetree (concat org-directory "/capture/todo.org"))
        "*TODO %?\n创建于： %U\n  %i\n  %a":clock-in t :clock-resume t)))

;;设置TODO
(setq org-todo-keywords '((sequence "TODO" "DOING" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-log-done 'note);;TODO 设置成完成时，会提示输入批注
(setq org-startup-indented t)

(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode 1)
                           (company-mode -1)                           
                           (local-set-key [tab] 'org-cycle)
                           (local-set-key [(control return)] 'set-mark-command)
                           (setq truncate-lines nil)))

;;设置日程视图
(setq org-agenda-files (list org-directory
                             "")) ;显示在日程里的文件路径目前只有包含在org-directory里头的，后续可以根据需要扩展


;;自定义org-bullet
(setq org-bullets-bullet-list
  '("☯"
    "☢"
    "◉"
    "○"
    "●"
    "✿"
    "♠"
    "♥"
    "♣"
    "♦"
    ;; ◇ ✚ ✜
    
    ;; ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
    ;; ► • ★ ▸
))



(provide 'org-setup)
;;; org-setup.el ends here
