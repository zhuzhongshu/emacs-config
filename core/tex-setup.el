(setq TeX-auto-save t)
(setq TeX-shell "/bin/bash")
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ;; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ;; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-toc-split-windows-horizontally t) ;;*toc*buffer在左侧。
(setq reftex-toc-split-windows-fraction 0.3)  ;;*toc*buffer 使用整个frame的比例。

;;将hyperref加入reference style以便使用\autoref等，注意要首先设置reftex-ref-style-alist,然后将键绑定到C-c C-\
(setq reftex-ref-style-alist (
                              quote (("Default" t (("\\ref" 114) ("\\pageref" 112) ("\\myref" 109) ("\\eqref" 101)))
                                     ("Varioref" "varioref" (("\\vref" 118) ("\\vpageref" 103) ("\\Vref" 86) ("\\Ref" 82)))
                                     ("Fancyref" "fancyref" (("\\fref" 102) ("\\Fref" 70)))
                                     ("Hyperref" "hyperref" (("\\autoref" 97) ("\\autopageref" 117))))))
(add-to-list 'reftex-ref-style-default-list "Hyperref")
(global-set-key [(control c) (control \\)] 'reftex-reference)   ;;默认定义是C-c C-)
;;(add-hook 'find-file-hook 'TeX-fold-buffer t)


(add-hook 'LaTeX-mode-hook (lambda ()
                             (rainbow-delimiters-mode 1)
                             (TeX-fold-mode 1)
                             (linum-mode 1)
                             ;;                             (auto-complete-mode 1);;添加ispell自动补全的ac-source
                             (LaTeX-math-mode 1)
                             (setq TeX-show-compilation nil)   ;;NOT display compilation windows
                             (setq TeX-clean-confirm nil)
                             (setq TeX-save-query nil)
                             (imenu-add-menubar-index)
                             (setq TeX-insert-braces  nil)
                             (setq TeX-electric-escape nil)     ;;按 \ 后光标跳到mini-buffer里面输入命令
                             (setq TeX-fold-env-spec-list       ;;设置执行折叠的项
                                   (quote (("[Comment]" ("comment"))
                                           ("[Equation]" ("equation"))
                                           ("[Figure]" ("figure"))
                                           ("[Table]" ("table"))
                                           ("[Itemize]"("itemize"))
                                           ("[Enumerate]"("enumerate"))
                                           ("[Description]"("description"))
                                           ("[Overpic]"("overpic"))
                                           ("[Shell]"("shell")))))
                             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                             ;;绝对路径,老版本的okular有个bug，标签模式下必须传递绝对路径才能打开文件
                             (add-to-list 'TeX-expand-list '("%O"
                                                             (lambda nil
                                                               (concat default-directory
                                                                       (funcall file
                                                                                (TeX-output-extension)
                                                                                nil nil)))
                                                             ))
                             (setq TeX-command-default "LatexMk");;设置C-c C-c默认编译命令为LatexMk
                             (setq TeX-view-program-list
                                   '(("SumatraPDF" "SumatraPDF.exe %o")
                                     ("Gsview" "gsview32.exe %(PDF)")
                                     ("Okular" "okular %O")
                                     ("Firefox" "firefox %o")))
                             (setq reftex-section-levels
                                   '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
                                     ("frametitle" . 4) ("subsubsection" . 4) ("paragraph" . 5)
                                     ("subparagraph" . 6) ("addchap" . -1) ("addsec" . -2)))));;设置更深层的目录

;;设置windows和linux下默认的编译器和文档查看软件
(cond
 ((eq system-type 'windows-nt)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "SumatraPDF")
                                                 (output-dvi "Yap"))))))
 ((eq system-type 'gnu/linux)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection '((output-pdf "Okular")
                                                 (output-dvi "Okular")))))))


;;以下是.latexmkrc文件中的代码，实现用xelatex编译，okular预览pdf
;; $pdflatex = "xelatex %O %S";
;; $pdf_mode = 1;
;; $postscript_mode = $dvi_mode = 0;
;; $preview_mode = 1;
;; $pdf_previewer = "start okular %O %S";

(provide 'tex-setup)
;;; init-tex.el ends here
