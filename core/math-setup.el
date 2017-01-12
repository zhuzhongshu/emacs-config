;;============================================================================================================
;;                                            octave-mode
;;============================================================================================================
;;run-octave命令不能用，网上搜到了一个解决方案：
;;1.在主文件夹下建立一个.octaverc文件，然后在里面输入 PS1(">> ") 并保存
;;2.在emacs配置文件中加上(setq inferior-octave-prompt ">> ") 并保存
;;其实">> "可以换成其他符号，只要保证两个命令一样就行了

;; autolad octave mode for *.m-files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (run-octave t)              ;;后台开启octave用于company补全
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(setq octave-comment-start "% ")
(setq inferior-octave-prompt ">> ")
;;配置octave-模式的ac-source，它会自动run-octave,用于代码编辑窗口
;; (require 'ac-octave)
;; (defun ac-octave-mode-setup ()
;;   (setq ac-sources '(ac-source-octave
;;                      ;; ac-source-yasnippet
;;                      ;; ac-source-filename
;;                      ;; ac-source-files-in-current-dir
;;                      ac-source-words-in-same-mode-buffers))
;;   (set-face-background 'ac-octave-candidate-face "#232526")
;;   (set-face-foreground 'ac-octave-candidate-face "#C68045")
;;   (set-face-background 'ac-octave-selection-face "#C68045")
;;   (set-face-foreground 'ac-octave-selection-face "#002B36"))
;; (add-hook 'octave-mode-hook
;;           '(lambda () (ac-octave-mode-setup)))
;; ;;为octave命令窗口添加补全，注意不是脚本窗口，利用的是complete-at-point补全源
;; (add-hook 'inferior-octave-mode-hook 'ac-capf-setup)

;;============================================================================================================
;;                                            imaxima
;;============================================================================================================
(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(require 'imaxima)
(setq imaxima-fnt-size "Huge")
(setq imaxima-print-tex-command "latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf")
(setq imaxima-equation-color "#D33682")
(setq imaxima-latex-preamble "\\usepackage{fourier}");;控制改变数学字体

(add-hook 'comint-mode-hook
          (lambda ()
            ;; (local-set-key [tab] 'maxima-complete )
            (setq comint-process-echoes nil)
            ));;comint-process-echos设置成t会与maxima冲突，导致其崩溃
(add-hook 'semantic-init-hooks 'my-semantic-hook)
(setq auto-mode-alist
      (append '(("\\.\\(mac\\|max\\)$" . maxima-mode)) auto-mode-alist))
;; (add-hook 'maxima-mode-hook 'auto-complete-mode)

(provide 'math-setup)
;;; math-setup.el ends here
