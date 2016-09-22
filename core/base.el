;; ;;; package --- Summary
;; ;; 基本上所有的配置都在这里了

;; ;;; Commentary:
;; ;; 插入时间的命令：C-u M-! date,也可以用org-time-stamp
;; ;; 最后修改时间 2015年 04月 03日 星期五 13:44:06 CST




;; ;; 如果有些包的选项不明白怎么用，或者不知道有哪些选项，可以试试M-x customize-variable 然后输入与包相关的关键词，说不定能找到

;; ;;; code:
;; ;;============================================================================================================
;; ;;                                            basic setup
;; ;;============================================================================================================
;;   (server-start)                          ;开启emacs服务器功能
;;   (fset 'yes-or-no-p 'y-or-n-p)           ;改变 Emacs 固执的要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
;;   (mouse-avoidance-mode 'animate)         ;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
;;   (setq x-gtk-use-system-tooltips nil)
;;   (tool-bar-mode -1)                      ;关闭工具栏
;;   (menu-bar-mode -1)                      ;开启菜单栏
;;   (display-time-mode 1)                   ;启用时间显示
;;   (auto-fill-mode 1)                      ;开启自动分行模式
;;   (setq-default fill-column 120)          ;可以使用M-q来对选中的段落手动分行，每行最多120个字符
;;   (setq-default word-wrap t)              ;开启word-wrap模式
;;   (setq cua-enable-cua-keys nil)          ;cua-mode时不覆盖原始emacs的键绑定
;;   (global-linum-mode t)                   ;显示行号
;;   (put 'upcase-region 'disabled nil)
;;   (put 'downcase-region 'disabled nil)
;;   (setq show-paren-mode t)
;;   (setq-default indent-tabs-mode  nil)    ;空格替换tab
;;   (setq backup-directory-alist '(("." . "~/.saves")));设置自动备份文件放在~/.saves目录下
;;   (setq auto-mode-alist
;;         (append '(("\\.\\(h\\|hpp\\|c\\)$" . c++-mode)) auto-mode-alist));;设置h文件和hpp文件用C++-mode，而不是默认的C-mode
;;   (when (fboundp 'winner-mode)
;;     (winner-mode)
;;     (windmove-default-keybindings))
;;   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;   ;;以下几个命令解决在大写状态下光标移动快捷键同时做标记的bug,并没有实际意义
;;   ;;目前并不清楚这个bug的起因是什么
;;   (global-set-key (kbd "C-S-p") 'previous-line)
;;   (global-set-key (kbd "C-S-n") 'next-line)
;;   (global-set-key (kbd "C-S-f") 'forward-char)
;;   (global-set-key (kbd "C-S-b") 'backward-char)
;;   (global-set-key (kbd "C-S-a") 'move-beginning-of-line)
;;   (global-set-key (kbd "C-S-e") 'move-end-of-line)
;;   (global-set-key (kbd "C-S-v") 'scroll-up-command)

;;   (global-set-key (kbd "M-F") 'forward-word)
;;   (global-set-key (kbd "M-B") 'backward-word)
;;   (global-set-key (kbd "M-A") 'backward-sentence)
;;   (global-set-key (kbd "M-E") 'forward-sentence)
;;   (global-set-key (kbd "M-V") 'scroll-down-command)
;; ;;注意，有部分选项是在emacs的GUI菜单里设置的，那些选项就包含在.emacs文件中而不是在这儿
;; ;;(setq inhibit-startup-message t)      ;关闭起动时的那个“开机画面”
;; ;; 时间设置放在.emacs里
;; ;; (setq display-time-24hr-format t)       ;时间使用24小时制
;; ;; (setq display-time-day-and-date t)      ;时间显示包括日期和具体时间
;; ;; (setq display-time-format "%Y-%m-%d %H:%M:%S")
;; ;;(setq display-time-use-mail-icon t)   ;时间栏旁边启用邮件设置
;; ;; (setq display-time-interval 1)         ;时间的变化频率，单位是秒
;; ;;(setq-default cursor-type 'bar)       ;设置光标为竖线


;; ;;============================================================================================================
;; ;;                                            global-variables
;; ;;============================================================================================================


;; ;;============================================================================================================
;; ;;                                            customize-themes
;; ;;============================================================================================================
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/");;添加主题文件夹
;; (load-theme 'monokai t);;选择主题为molokai
;; (column-number-mode t);;在mode-line处显示列号
;; (line-number-mode t);;在mode-line处显示行号
;; (setq linum-format "%3i");;设置左侧行号格式，在格式化字符串内加空格可以体现在行号上，如" %3i% "
;; (setq c-default-style "ellemtel");;设置缩进格式为ellemtel
;; ;; 字体设置
;; (prefer-coding-system 'utf-8)
;; (set-language-environment 'UTF-8)
;; (set-locale-environment "UTF-8")

;; (set-frame-font "Monaco 16")
;; ;; (set-frame-font "monacoyahei 16")       ;这个字体会导致popup对齐出错
;; ;; (set-frame-font "Consolas 16")
;; ;; (set-frame-font "inconsolata 19")

;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (set-fontset-font t 'unicode "Droid Sans Fallback 17"))))
;;   (set-fontset-font t 'unicode "Droid Sans Fallback  17"))
;; ;;============================================================================================================
;; ;;                                            
;; ;;============================================================================================================
;; (setq my-home-dir (getenv "HOME"))

;; ;;============================================================================================================
;; ;;                                            clean-mode-line
;; ;;============================================================================================================
;; (defvar mode-line-cleaner-alist
;;   '((auto-complete-mode . "")
;;     ;; Major modes
;;     (company-mode . "C")
;;     (emacs-lisp-mode . "EL")
;;     )
;;   "Alist for `clean-mode-line'.
;; When you add a new element to the alist, keep in mind that you
;; must pass the correct minor/major mode symbol and a string you
;; want to use in the modeline *in lieu of* the original.")

;; (defun clean-mode-line ()
;;   (interactive)
;;   (loop for cleaner in mode-line-cleaner-alist
;;         do (let* ((mode (car cleaner))
;;                  (mode-str (cdr cleaner))
;;                  (old-mode-str (cdr (assq mode minor-mode-alist))))
;;              (when old-mode-str
;;                  (setcar old-mode-str mode-str))
;;                ;; major mode
;;              (when (eq mode major-mode)
;;                (setq mode-name mode-str)))))

;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)
(require 'init-basic)


;;============================================================================================================
;;                                            ein
;;============================================================================================================
(defvar python-executable-path);;仅仅是为了防止出现 assign to free variable的警告...
(setq python-executable-path "/opt/anaconda3/bin/");;python可执行程序的目录，ein和elpy采用的是anaconda自带的python，比较适合于科学计算
(require 'ein)
;; (setq ein:console-executable "/usr/bin/ipython3")
(setq ein:console-executable
      (concat python-executable-path "ipython3"))
;; (setq ein:console-executable "/opt/anaconda3/bin/ipython3")
;;============================================================================================================
;;                                            elpy
;;============================================================================================================
(require 'elpy)
(elpy-enable);;对所有python文件都开启elpy模式，该函数自动将elpy-mode加入了python-mode-hook，还做了其他设置
(setq elpy-rpc-python-command
      (concat python-executable-path "python3"));;设置python后台程序为python3
;;使用ipython3作为interactive shell
(elpy-use-ipython
 (concat python-executable-path "ipython3"))
(setq elpy-modules '(elpy-module-eldoc
                     ;; elpy-module-company ;;补全用company-ycmd
                     ;; elpy-module-flymake ;;语法检查用flycheck
                     elpy-module-yasnippet  ;;elpy自己提供了一些snippet
                     elpy-module-pyvenv
                     elpy-module-highlight-indentation
                     elpy-module-sane-defaults))
(define-key elpy-mode-map (kbd "<C-return>") nil);;执行当前语句的命令跟标记冲突，重新绑定
(define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-current-statement);;执行当前语句
(define-key elpy-mode-map (kbd "<f10>")'elpy-format-code);;格式化代码，与clang-format一致


;;============================================================================================================
;;                                            browse-kill-ring
;;============================================================================================================
;; 可以用helm代替
;; (browse-kill-ring-default-keybindings)
;; (custom-set-variables
;;  '(ansi-color-names-vector
;;    ["#002B36" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#7A9496"])
;;  '(browse-kill-ring-display-duplicates nil)
;;  '(browse-kill-ring-highlight-current-entry nil)
;;  '(browse-kill-ring-separator "-------------------------------------------"))





;;============================================================================================================
;;                                            ace-jump-mode
;;============================================================================================================
;; (autoload
;;   'ace-jump-mode
;;   "ace-jump-mode"
;;   "Emacs quick move minor mode"
;;   t)
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; (autoload
;;   'ace-jump-mode-pop-mark
;;   "ace-jump-mode"
;;   "Ace jump back:-)"
;;   t)
;; (eval-after-load "ace-jump-mode"
;;   '(ace-jump-mode-enable-mark-sync))
;; (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; (setq ace-jump-mode-submode-list
;;       '(ace-jump-char-mode              ;; the first one always map to : C-c SPC
;;         ace-jump-word-mode              ;; the second one always map to: C-u C-c SPC
;;         ace-jump-line-mode) )           ;; the third one always map to ：C-u C-u C-c SPC
;; (setq ace-jump-mode-move-keys
;;       (nconc (cl-loop for i from ?a to ?z collect i))) ;要用cl-loop替代项目主页上的loop










;;============================================================================================================
;;                                            yasnippet
;;============================================================================================================

;;============================================================================================================
;;                                            smex
;;============================================================================================================

;; ;;============================================================================================================
;; ;;                                            auto-complete-mode
;; ;;============================================================================================================
;; ;; 额安装了一些补全源，这些源只在特殊模式下才会使用，相应的其他全局补全源也有可能被重新设置而覆盖，
;; ;; 这些补全源的特殊配置可以参考各个源的配置代码部分，不再此处
;; ;; clang-async->只用与C语言系列模式
;; ;; ispell->只用与文档编写模式:text-mode latex-mode mail-mode
;; ;; readline->只用于shell-mode，
;; ;; octave->只用于octave-mode，
;; ;; complete-at-point->只用于octave的命令窗口inferior-octave-mode
;;  ;; (require 'pos-tip)
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140824.1658/dict")
;; (ac-config-default)
;; (ac-flyspell-workaround);;修正ac和flyspell冲突的问题
;; ;;自定义弹出窗口的颜色,对于内置源之外的补全源，要在调用的时候定义颜色，否则会报错
;; ;;内置源的配色已经挪到主题中定制
;; ;; (set-face-background 'ac-candidate-face "#232526");;monokai-hl
;; ;; (set-face-foreground 'ac-candidate-face "#2AA198");;solarized-cyan
;; ;; (set-face-background 'ac-selection-face "#2AA198");;solarized-cyan
;; ;; (set-face-foreground 'ac-selection-face "black")
;; ;; (set-face-foreground 'ac-completion-face "#D33682");;solarized-magenta
;; ;; (set-face-underline 'ac-completion-face "#66D9EF");;blue(not solarized)

;; ;; ;;yasnippet
;; ;; (set-face-background 'ac-yasnippet-candidate-face "#232526");;monokai-hl
;; ;; (set-face-foreground 'ac-yasnippet-candidate-face "#D33682");;solarized-magenta
;; ;; (set-face-background 'ac-yasnippet-selection-face "#D33682");;solarized-magenta
;; ;; (set-face-foreground 'ac-yasnippet-selection-face "#002B36");;momokai-bg

;; ;;此处是ac-sources的全局设置，默认的sources比较少，像yasnippet和文件名这些就没有。注意要加在(ac-config-default)之后以覆盖默认设置，当然也可以用append添加
;; (setq-default ac-sources '(ac-source-yasnippet
;;                            ac-source-words-in-same-mode-buffers
;;                            ac-source-dictionary
;;                            ac-source-filename
;;                            ac-source-files-in-current-dir))
;; (setq ac-auto-start nil);;关闭自动触发补全
;; (ac-set-trigger-key "<tab>");;绑定补全触发键为Tab
;; (setq ac-use-menu-map t)
;; (define-key ac-completing-map "\C-n" 'ac-next);;让C-n C-p也能用来选择补全候选项
;; (define-key ac-completing-map "\C-p" 'ac-previous)

;; ;;============================================================================================================
;; ;;                                            auto-complete-clang-async
;; ;;============================================================================================================
;; ;;这个从MELPA上下载的没有编译成可执行文件的部分，还是要到github去下载编译
;; (require 'auto-complete-clang-async)
;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "~/.emacs.d/elpa/auto-complete-clang-async-20130526.814/clang-complete");;此处是可执行文件的路径
;;   (setq ac-sources '(ac-source-clang-async
;;                      ac-source-yasnippet
;;                      ac-source-filename
;;                      ac-source-files-in-current-dir));;这里定义的补全source可以覆盖全局设置
;;   (setq ac-clang-cflags (quote ("-std=c++11")));;使clang支持C++11，否则很多新函数补不出来，其他flags可以仿此设置
;;   (ac-clang-launch-completion-process)
;;   )

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'c-mode-common-hook(lambda ()
;;                                  (local-set-key (kbd "M-<tab>") 'ac-complete-clang-async)))
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (set-face-background 'ac-clang-candidate-face "#232526")
;;   (set-face-foreground 'ac-clang-candidate-face "#2AA198")
;;   (set-face-background 'ac-clang-selection-face "#D33682")
;;   (set-face-foreground 'ac-clang-selection-face "#002B36")
;;   (global-auto-complete-mode t))
;; (my-ac-config)
;;============================================================================================================
;;                                            auto-complete-ispell
;;============================================================================================================
;; (custom-set-variables
;;  '(ac-ispell-requires 4);;设置至少输入4个字符后才能启动该ac-source，并不会影响其他source
;;  '(ac-ispell-fuzzy-limit 5));;设置模糊匹配单词的个数

;; ;; (eval-after-load "auto-complete"
;; ;;   '(progn
;; ;;      (ac-ispell-setup)
;; ;;      (set-face-foreground 'ac-ispell-fuzzy-candidate-face "#859900")))
;; ;;以下三个模式的ac源在默认的基础上添加了ispell补全
;; (add-hook 'text-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'LaTeX-mode-hook 'ac-ispell-ac-setup)
;; ;;===============================================
;;=============================================================
;;                                            shell-mode
;;============================================================================================================
;; ;;shell-mode启用了自动补全，补全源只有一个：ac-source-shell
;; (require 'readline-complete)
;; (setq explicit-shell-file-name "bash")
;; (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
;; ;; 由于imaxima也用的是comint-mode，所以下面这个变量设置成t会影响imaxima的正常运行。
;; ;; 好在comint.el中指出这是个buffer-local的变量，因此可以在不同buffer里设置不同的值，方法就是用mode-hook
;; ;; 在shell-mode里设置成t，而在imaxima里设置成false
;; ;; (setq comint-process-echoes t)
;; (add-to-list 'ac-modes 'shell-mode)
;; ;; 设置shell-mode的补全源
;; (defun my-ac-rlc-setup-sources ()
;;   (setq ac-sources '(ac-source-shell));;这里定义的补全source可以覆盖全局设置
;;   (add-hook 'rlc-no-readline-hook '(lambda () (auto-complete-mode -1))))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;============================================================================================================
;;                                            xscope
;;============================================================================================================


;;============================================================================================================
;;                                            popup-switcher
;;============================================================================================================
;; (require 'popup-switcher)
;; (setq psw-use-flx t)
;; (setq psw-in-window-center nil)
;; (global-set-key "\M-[" 'psw-switch-buffer)






;;============================================================================================================
;;                                            pdf-tools
;;============================================================================================================
;;(pdf-tools-install)


;;缩进整个buffer
;; (defun iwb ()
;;   "indent whole buffer"
;;   (interactive)
;;   (delete-trailing-whitespace)
;;   (indent-region (point-min) (point-max) nil)
;;   (untabify (point-min) (point-max)))
;; (global-set-key [f9] 'iwb)
;; ;; 自定义全屏函数,在24.4版本中默认已经有了全屏快捷键，就是F11
;; ;; (defun fullscreen ()
;; ;;   (interactive)
;; ;;   (set-frame-parameter nil 'fullscreen
;; ;;                        (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;; ;; (global-set-key [f11] 'fullscreen)
;; ;; C=l清屏函数
;; (defun clear-shell ()
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))
;; (add-hook 'shell-mode-hook(lambda ()
;;                             (local-set-key (kbd "C-l") 'clear-shell)))
;; (add-hook 'inferior-octave-mode-hook(lambda ()
;;                                       (local-set-key (kbd "C-l") 'clear-shell)))
;; (add-hook 'comint-mode-hook(lambda ()
;;                              (local-set-key (kbd "C-l") 'clear-shell)))
;; (add-hook 'comint-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "M-h") 'helm-comint-input-ring )));;comint-mode里的输入历史

;; ;; 调用goldendict查询单词，借用了zeal-at-point中的代码
;; (defun golden-at-point-run-search (search)
;;   (if (executable-find "goldendict")
;;       (start-process "Golden" nil "goldendict" search)
;;     (message "GoldenDict wasn't found"))
;;   )
;; (defun golden-at-point (&optional edit-search)
;;   "Search for the word at point in Golden.  EDIT-SEARCH."
;;   (interactive "P")
;;   (let* ((thing (if mark-active
;;                     (buffer-substring (region-beginning) (region-end))
;;                   (thing-at-point 'symbol)))
;;          (search thing))
;;     (golden-at-point-run-search
;;      (if (or edit-search (null thing))
;;          (read-string "Golden search: " search)
;;        search))))
;; (global-set-key [(control c) (s)] 'golden-at-point)
;;============================================================================================================
;;                                            keyboadr macros
;;============================================================================================================
;; 可以作为不会elisp写代码的一个补偿方案
;; 录制macro的命令（可以用kmacro）：
;;  开始录制宏：kmacro-start-macro       C-x (
;;  录制结束：kmacro-end-macro           C-x )
;;  给宏命名：kmacro-name-last-macro
;;  得到对应代码：insert-kbd-macro
;; 然后将得到的代码插入到这里就，以后再想使用就直接用M-x 宏名字就可以了。

;; 将窗口左右均分，右端窗口开启shell，然后将窗口布局存入到寄存器‘s’中
;; (fset 'zzsshell
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p")
;;         (kmacro-exec-ring-item (quote ([24 51 134217778 134217848 115 104 101 108 108 return 24 114 119 115] 0 "%d")) arg)))
;; ;; 将窗口左右均分，右端窗口开启octave，然后将窗口布局存入到寄存器‘o’中
;; (fset 'zzsoctave
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p")
;;         (kmacro-exec-ring-item (quote ([134217848 114 117 110 111 99 116 97 118 101 return
;;                                                   134217777 24 49 24 51 134217778 24 98 111 99 116 97 118 101 return
;;                                                   134217777 24 114 119 111] 0 "%d")) arg)))






(provide 'base.el)
;;; base.el ends here


