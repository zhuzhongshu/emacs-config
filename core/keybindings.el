;;============================================================================================================
;;                                            global shortcuts
;;============================================================================================================

;;FX系列已经绑定的快捷键：
;;f7 ispell-word 单词拼写检查
;;f8 languagetool 语法检查
;;f9 iwb 缩进整个buffer
;;f10 fullscreen 全屏
;;f11 clang-format 格式化c代码，只在c-mode下有效

(global-set-key [(control return)] 'set-mark-command)         ;;与C-@功能一样，设置选择文本的起点
(global-set-key [(control x) (control /)] 'comment-region)    ;;注释选中区域，快捷绑定为C-x C-/
(global-set-key [(control x) (control \\)] 'uncomment-region)   ;;反注释选中区域，快捷绑定为C-x C-\， 注意反斜线要用\\表示
(global-set-key [(control x) (control /)] 'comment-dwim-2)
(global-set-key [(control x) (control a)] 'revert-buffer)
(global-set-key (kbd "C-\\") 'delete-whitespace-rectangle)
;;以下几个命令解决在大写状态下光标移动快捷键同时做标记的bug,并没有实际意义
;;目前并不清楚这个bug的起因是什么
(global-set-key (kbd "C-S-p") 'previous-line)
(global-set-key (kbd "C-S-n") 'next-line)
(global-set-key (kbd "C-S-f") 'forward-char)
(global-set-key (kbd "C-S-b") 'backward-char)
(global-set-key (kbd "C-S-a") 'move-beginning-of-line)
(global-set-key (kbd "C-S-e") 'move-end-of-line)
(global-set-key (kbd "C-S-v") 'scroll-up-command)
(global-set-key (kbd "M-F") 'forward-word)
(global-set-key (kbd "M-B") 'backward-word)
(global-set-key (kbd "M-A") 'backward-sentence)
(global-set-key (kbd "M-E") 'forward-sentence)
(global-set-key (kbd "M-V") 'scroll-down-command)

(provide 'keybindings)
;;; keybindings.el ends here
