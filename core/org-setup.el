;;============================================================================================================
;;                                            org-mode
;;============================================================================================================
(require 'org-install)
;; (require 'org-publish)



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
(require 'org-bullets)
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
    ;; ► • ★ ▸
))

;;导出latex和pdf的设置
(require 'ox-latex)


;;自定义一个class并设置其为默认class
(add-to-list 'org-latex-classes '("article-zh" "\\documentclass[a4paper,10pt]{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\setCJKfamilyfont{songti}{宋体}
\\newcommand*{\\song}{\\CJKfamily{songti}}
\\setCJKfamilyfont{heiti}{黑体}
\\newcommand*{\\hei}{\\CJKfamily{heiti}}
\\setCJKfamilyfont{kaiti}{楷体}
\\newcommand*{\\kai}{\\CJKfamily{kaiti}}
\\setCJKfamilyfont{lishu}{隶书}
\\newcommand*{\\li}{\\CJKfamily{lishu}}
\\setCJKfamilyfont{youyuan}{幼圆}
\\newcommand*{\\you}{\\CJKfamily{youyuan}}
\\setmainfont{Nimbus Roman}
\\setmonofont{Monaco}
\\definecolor{bg}{RGB}{0,43,54}
\\usemintedstyle{monokai}
\\hypersetup{hidelinks}"

                                 ("\\section{%s}" . "\\section*{%s}")
                                 ("\\subsection{%s}" . "\\subsection*{%s}")
                                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-default-class "article-zh")

;;设置latex默认加入的包
(setq org-latex-default-packages-alist '(("UTF8, heading=true" "ctex" t)
                                          ("" "fontspec" t)
                                          ("top=1in, bottom=1in, left=1.25in, right=1.25in" "geometry" t)
                                          ("" "minted")                                          
                                          ("AUTO" "inputenc" t)
                                          ("" "xcolor" t)
                                          ("" "bm" t)
                                          ("" "fixltx2e" nil)
                                          ("" "graphicx" t)
                                          ("" "longtable" nil)
                                          ("" "float" nil)
                                          ("" "wrapfig" nil)
                                          ("" "rotating" nil)
                                          ("normalem" "ulem" t)
                                          ("" "amsmath" t)
                                          ("" "textcomp" t)
                                          ("" "marvosym" t)
                                          ("" "wasysym" t)
                                          ("" "amssymb" t)
                                          ("unicode=true,colorlinks=no,pdfborder=no" "hyperref" nil)
                                          "\\tolerance=1000"))

;;用minted实现语法高亮，需要在latexmkrc文件中设置xelatex的运行选项带有-shell-escape
;;另外还需要安装pygmentize，一个python包，可以用pip或者软件包管理器安装

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-minted-langs '(python "python")) ;增加python语言支持
;;minted的选项。如bgcolor=bg就写成("bgcolor" "bg")
(setq org-latex-minted-options
      '(("bgcolor" "bg")
        ("linenos" "true")
        ("breaklines" "true")
        ("breakautoindent" "true")
        ("breakanywhere" "true")
        ("fontsize" "\\footnotesize")))

;;导出pdf是先根据上面的设置导出tex文件，然后调用latexmk自动执行编译
(setq org-latex-pdf-process '("latexmk  %f"))


(provide 'org-setup)
;;; org-setup.el ends here
