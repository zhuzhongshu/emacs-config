(fset 'yes-or-no-p 'y-or-n-p)           ;按 y 或空格键表示 yes，n 表示 no。
(mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq x-gtk-use-system-tooltips nil)
(tool-bar-mode -1)                      ;关闭工具栏
(menu-bar-mode -1)                      ;开启菜单栏
(display-time-mode 1)                   ;启用时间显示
(auto-fill-mode 1)                      ;开启自动分行模式
(setq-default fill-column 120)          ;可以使用M-q来对选中的段落手动分行，每行最多120个字符
(setq-default word-wrap t)              ;开启word-wrap模式
(setq cua-enable-cua-keys nil)          ;cua-mode时不覆盖原始emacs的键绑定
(global-linum-mode t)                   ;显示行号
(setq show-paren-mode t)
;;(setq inhibit-startup-message t)      ;关闭起动时的那个“开机画面”

(setq display-time-24hr-format t)       ;时间使用24小时制
(setq display-time-day-and-date t)      ;时间显示包括日期和具体时间
(setq display-time-format "%Y-%m-%d %H:%M:%S")
;;(setq display-time-use-mail-icon t)   ;时间栏旁边启用邮件设置
(setq display-time-interval 1)         ;时间的变化频率，单位是秒
;; (setq-default cursor-type 'bar)       ;设置光标为竖线
(setq display-time-default-load-average nil)
(display-time)
(setq comint-prompt-read-only t)
;;============================================================================================================
;;                                            theme-and-font
;;============================================================================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/");;添加主题文件夹
(load-theme 'monokai t);;选择主题为molokai
(column-number-mode t);;在mode-line处显示列号
(line-number-mode t);;在mode-line处显示行号
(setq linum-format "%3i");;设置左侧行号格式，在格式化字符串内加空格可以体现在行号上，如" %3i% "
(setq c-default-style "ellemtel");;设置缩进格式为ellemtel
;; 字体设置
(prefer-coding-system 'utf-8)
(set-language-environment 'UTF-8)
(set-locale-environment "UTF-8")

(set-frame-font "Monaco 16")
;; (set-frame-font "monacoyahei 16")       ;这个字体会导致popup对齐出错
;; (set-frame-font "Consolas 16")
;; (set-frame-font "inconsolata 19")

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-fontset-font t 'unicode "Droid Sans Fallback 17"))))
  (set-fontset-font t 'unicode "Droid Sans Fallback  17"))

(provide 'appearance-setup)
;;; appearance-setup.el ends here
