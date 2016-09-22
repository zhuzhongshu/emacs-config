;;; package --- Summary
;; 基本上所有的配置都在这里了

;;; Commentary:
;; 插入时间的命令：C-u M-! date,也可以用org-time-stamp
;; 最后修改时间 2015年 04月 03日 星期五 13:44:06 CST




;; 如果有些包的选项不明白怎么用，或者不知道有哪些选项，可以试试M-x customize-variable 然后输入与包相关的关键词，说不定能找到

;;; code:
;;============================================================================================================
;;                                            basic setup
;;============================================================================================================
  (server-start)                          ;开启emacs服务器功能
  (fset 'yes-or-no-p 'y-or-n-p)           ;改变 Emacs 固执的要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
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
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (setq show-paren-mode t)
  (setq-default indent-tabs-mode  nil)    ;空格替换tab
  (setq backup-directory-alist '(("." . "~/.saves")));设置自动备份文件放在~/.saves目录下
  (setq auto-mode-alist
        (append '(("\\.\\(h\\|hpp\\|c\\)$" . c++-mode)) auto-mode-alist));;设置h文件和hpp文件用C++-mode，而不是默认的C-mode
  (when (fboundp 'winner-mode)
    (winner-mode)
    (windmove-default-keybindings))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
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
;;注意，有部分选项是在emacs的GUI菜单里设置的，那些选项就包含在.emacs文件中而不是在这儿
;;(setq inhibit-startup-message t)      ;关闭起动时的那个“开机画面”
;; 时间设置放在.emacs里
;; (setq display-time-24hr-format t)       ;时间使用24小时制
;; (setq display-time-day-and-date t)      ;时间显示包括日期和具体时间
;; (setq display-time-format "%Y-%m-%d %H:%M:%S")
;;(setq display-time-use-mail-icon t)   ;时间栏旁边启用邮件设置
;; (setq display-time-interval 1)         ;时间的变化频率，单位是秒
;;(setq-default cursor-type 'bar)       ;设置光标为竖线


;;============================================================================================================
;;                                            global-variables
;;============================================================================================================


;;============================================================================================================
;;                                            customize-themes
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
;;============================================================================================================
;;                                            
;;============================================================================================================
(setq my-home-dir (getenv "HOME"))

;;============================================================================================================
;;                                            clean-mode-line
;;============================================================================================================
(defvar mode-line-cleaner-alist
  '((auto-complete-mode . "")
    ;; Major modes
    (company-mode . "C")
    (emacs-lisp-mode . "EL")
    )
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)



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
;;                                            power-line
;;============================================================================================================

(require 'powerline)
(defun powerline-my-theme ()
  "Setup a mode-line with major and minor modes centered."
  (interactive)
  (setq-default mode-line-format
		'("%e"
		  (:eval
		   (let* ((active (powerline-selected-window-active))
			  (mode-line (if active 'mode-line 'mode-line-inactive))
			  (face1 (if active 'powerline-active1 'powerline-inactive1))
			  (face2 (if active 'powerline-active2 'powerline-inactive2))
			  (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
							  (car powerline-default-separator-dir))))
			  (separator-right (intern (format "powerline-%s-%s"
							   (powerline-current-separator)
							   (cdr powerline-default-separator-dir))))
			  (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
				     (powerline-buffer-id nil 'l)
                                     (powerline-raw " " nil 'l)                                     
				     (funcall separator-left mode-line face1)
                                     (powerline-raw "  " face1 'l)
				     ;; (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
			  (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw " " face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw "(%1l" nil )
                                     (powerline-raw "," nil )
				     (powerline-raw "%1c)" nil 'r)
				     (powerline-raw "%6p " nil 'r)
                                     (powerline-hud face2 face1)
                                     ))
                          (center (list (powerline-raw " " face1)
					(funcall separator-left face1 face2)
					(when (boundp 'erc-modified-channels-object)
					  (powerline-raw erc-modified-channels-object face2 'l))
					(powerline-major-mode face2 'l)
					(powerline-process face2)
					(powerline-raw " :" face2)
					(powerline-minor-modes face2 'l)
					(powerline-raw " " face2)
					(funcall separator-right face2 face1))))
		     (concat (powerline-render lhs)
			     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
			     (powerline-render center)
			     (powerline-fill face1 (powerline-width rhs))
			     (powerline-render rhs)))))))
(powerline-my-theme)

;;============================================================================================================
;;                                            spell-check
;;============================================================================================================
(setq-default ispell-program-name "aspell");;设置拼写检查的软件为aspell
(setq current-language-environment "en_US");;设置当前语言环境为en_US
(ispell-change-dictionary "en_US" t);;设置拼写检查的辞典为en_US:emacs24.3自动安装有ispell和辞典
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(global-set-key (kbd "<f7>") 'ispell-word)                      ;;F7键修改光标处的单词拼写,默认定义是M-$
(global-set-key (kbd "M-<f7>") 'flyspell-mode)          ;;开启/关闭flyspell mode
(global-set-key (kbd "C-<f7>") 'flyspell-buffer)
;;============================================================================================================
;;                                           grammar-check
;;============================================================================================================
(require 'langtool)
(setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-2.6/languagetool-commandline.jar"
      langtool-mother-tongue "en"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))
(global-set-key (kbd "<f8>") 'langtool-check)           ;;首先摁F8开始语法检查，然后C-F8修正，最后M-F8退出
(global-set-key (kbd "C-<f8>") 'langtool-correct-buffer)
(global-set-key (kbd "M-<f8>") 'langtool-check-done)
(global-set-key (kbd "S-<f8>") 'langtool-show-message-at-point)
(global-set-key (kbd "C-S-<f8>") 'langtool-switch-default-language)
;;============================================================================================================
;;                                            electric-mode
;;============================================================================================================
(require 'electric)
(setq electric-indent-mode nil)                ;;开启缩进功能，用enter键也能取得C-j的效果
(setq electric-pair-mode nil);;系统本身内置的智能自动补全括号,用smartparens代替

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
;;                                            Tag Folding
;;============================================================================================================
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(global-set-key [(control <)] 'hs-hide-block)                   ;;以下四个为代码折叠快捷键
(global-set-key [(control >)] 'hs-show-block)
(global-set-key [(control x) (control <)] 'hs-hide-all)
(global-set-key [(control x) (control >)] 'hs-show-all)

;;============================================================================================================
;;                                            vimish fold
;;============================================================================================================
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c f") 'vimish-fold)
;;============================================================================================================
;;                                            auto-compile
;;============================================================================================================
;;自动编译改动后的el文件
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)
(setq auto-compile-display-buffer t)
(setq auto-compile-mode-line-counter t)



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
;;                                            avy mode
;;============================================================================================================
(avy-setup-default)
(setq avy-background t)
(setq avy-keys (number-sequence ?a ?z))
;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))     
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-c w") 'avy-goto-word-1)

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
;; company-ycmd配置，速度方面比irony快不少
(require 'ycmd)

;;ycmd目前不支持anaconda
(set-variable 'ycmd-server-command '("python"  "/home/simplex/.emacs.d/ycmd/ycmd/"))
;; (set-variable 'ycmd-server-command `(,(concat python-executable-path "python3") "/home/simplex/.emacs.d/ycmd/ycmd/"))
(set-variable 'ycmd-global-config  "/home/simplex/.emacs.d/ycmd/cpp/ycm/.ycm_extra_conf.py");;额外的配置
(set-variable 'ycmd-extra-conf-whitelist '( "/home/simplex/.emacs.d/ycmd/cpp/ycm"))
;; (setq ycmd-python-binary-path  (concat python-executable-path "python3"))
(setq ycmd-python-binary-path "python3")


(global-ycmd-mode)
(require 'company-ycmd)
;; (require 'company-ycmd-ext)
(company-ycmd-setup)
(setq url-show-status nil);; 关掉minibuf里一直显示contacting host 127.0.0.1:xxxx的提示
(setq company-ycmd-enable-fuzzy-matching t);;开启fuzzy matching，可能会有点慢
;;默认值是2，输入两个字符就会自动启动ycmd补全，官方建议改的很大以防止这种情况的出现，同时不会影响输入::或->自动触发补全，
(setq ycmd-min-num-chars-for-completion 999)
(add-hook 'c-mode-common-hook
          (lambda ()
            (set (make-local-variable 'company-backends)  (quote (company-ycmd
                                                                  company-yasnippet
                                                                  company-files
                                                                  )))))

(add-hook 'LaTeX-mode-hook (lambda ()
                           (setq-local company-backends
                                       (append '(company-math-symbols-latex company-latex-commands)
                                               company-backends))))
;; 将下面的代码替换company-template中的同样定义来改变快捷键的绑定
(setq company-template-nav-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [tab] 'company-other-backend)
    (define-key keymap [(meta tab)] 'company-template-forward-field)
    (define-key keymap (kbd "M-TAB") 'company-template-forward-field)
    keymap))

;;============================================================================================================
;;                                            hippie-expand
;;============================================================================================================

(setq hippie-expand-try-functions-list '(try-expand-whole-kill
                                         try-expand-line                    ; 补全当前行
                                         try-expand-dabbrev                 ; 搜索当前 buffer
                                         try-expand-dabbrev-visible         ; 搜索当前可见窗口
                                         try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
                                         try-expand-dabbrev-from-kill       ; 从 kill-ring 中搜索
                                         try-complete-file-name-partially   ; 文件名部分匹配
                                         try-complete-file-name             ; 文件名匹配
                                         try-expand-all-abbrevs             ; 匹配所有缩写词
                                         try-expand-list))


;;============================================================================================================
;;                                            helm-mode&plugins
;;============================================================================================================
(require 'helm-config)

(helm-mode 1)
(helm-autoresize-mode 1)


(custom-set-variables
 '(helm-buffer-max-length 30)
 '(helm-buffers-fuzzy-matching t)
 '(helm-locate-fuzzy-match t)
 '(helm-M-x-fuzzy-match t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*Messages")))
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file . ido))))
 '(helm-display-source-at-screen-top nil))
;;=========helm-gtags=========
;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t))
;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "\C-cgt") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "\C-cgr") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "\C-cgs") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "\C-cgp") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "\C-cg<") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "\C-cg>") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "\C-cg,") 'helm-gtags-pop-stack)))
;;==========swiper-hepm==========
;; (global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-s") 'swiper)
;; disable pre-input
(setq helm-swoop-pre-input-function
      (lambda () ""))
(global-set-key (kbd "C-r") 'helm-ag)
(global-set-key (kbd "M-x") 'helm-M-x);;大多数情况下helm-M-x可以工作的很好，但是它不支持用C-u传入参数，此时要用smex，绑定到了C-M-x
(global-set-key "\M-[" 'helm-buffers-list);;用helm切换buffer
(global-set-key "\M-]" 'helm-register);;用helm切换buffer
(global-set-key "\M-y" 'helm-show-kill-ring);;C-x b的另一个绑定
(define-key global-map (kbd "C-c C-r") 'helm-resume)
;;==========helm-bibtex==========
(require 'helm-bibtex)
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography '("/media/simplex/E/Doc/zotero/simplex.bib"))
(setq helm-bibtex-library-path '("/media/simplex/E/Doc/zotero/"))
(setq helm-bibtex-additional-search-fields '(journal))
(setq helm-bibtex-pdf-open-function
  (lambda (fpath)
    (start-process "okular" "*helm-bibtex-okukar*" "/usr/bin/okular" fpath)))
;;自定义文章名称，作者，期刊等的颜色
(defface helm-bibtex-title-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The title face for `helm-bibtex' .")

(defface helm-bibtex-author-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The author face for `helm-bibtex' .")

(defface helm-bibtex-journal-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The jornal face for `helm-bibtex' .")
(defface helm-bibtex-year-face
  '((t (:background "yellow" :foreground "black"))
    :group 'helm-bibtex)
  "The year face for `helm-bibtex' .")

(defun helm-bibtex-candidates-formatter (candidates source)
 "自己改写的函数，替换了在helm窗口中显示的内容，增加了期刊名，去掉了附件信息并添加了色彩
  颜色使用上面定义的几个变量，把它替换helm-bibtex.el中的对应函数即可。"
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (helm-bibtex-get-value "=key=" entry)
   if (assoc-string "author" entry 'case-fold)
   for fields = '("title" "year" "author" "journal"  )
   else
   for fields = '("title" "year" "editor"  "journal" )
   for fields = (-map (lambda (it)
                        (helm-bibtex-clean-string
                          (helm-bibtex-get-value it entry " ")))
                      fields)
   ;;处理作者项
   for fields = (-update-at 1 'helm-bibtex-shorten-authors fields)
   ;;分别给这几项字符串加上颜色
   for fields = (-update-at 0 (lambda (str) (propertize str 'face 'helm-bibtex-title-face)) fields)
   for fields = (-update-at 1 (lambda (str) (propertize str 'face 'helm-bibtex-year-face)) fields)
   for fields = (-update-at 2 (lambda (str) (propertize str 'face 'helm-bibtex-author-face)) fields)
   for fields = (-update-at 3 (lambda (str) (propertize str 'face 'helm-bibtex-journal-face)) fields)      
   collect
   ;;这个长度调整起来比较麻烦，不同显示器可能还得做不同的设置
   (cons (s-format " $0\t$1\tAuthor : $2\tJournal: $3" 'elt
                   (-zip-with (lambda (f w) (truncate-string-to-width f w 0 ?\s ))
                              fields (list (- width 20) 4 (- width 25) (- width 15) )))
         entry-key)))
(defun helm-bibtex-open-file-zzs (_)
  "自定义的打开附件函数.
zotero导出的bib有个file关键词,指出了附件的位置，可以根据这个来打开条目相应的附件.
目前支持pdf和html,因为基本只有这两种附件类型，"
  (let ((keys (helm-marked-candidates :with-wildcard t)))
    (dolist (key keys)
      (let* ((entry (helm-bibtex-get-entry key))
             (file-str (cdr (assoc "file" entry)))               
             (file-full-name (progn
                              (string-match ":\\([^:]*\\):" file-str)
                              ;;看要不要加上helm-bibtex-library-path
                              ;; (concat (car helm-bibtex-library-path)  (match-string 1 file-str))
                              (match-string 1 file-str)))
             (file-type (progn
                          ;;如果同时有pdf和html，则匹配第一个出现的文件类型
                          (string-match "\.\\([[:alpha:]]*\\)$" file-full-name)
                          (match-string 1 file-full-name))))
             (if (string= file-type "pdf")
                 (-each (list (concat "/media/simplex/E/Doc/zotero/" file-full-name)) helm-bibtex-pdf-open-function)
               (if (string= file-type "html")
                    (start-process "firefox" "*helm-bibtex-firefox*" "/usr/bin/firefox" (concat "/media/simplex/E/Doc/zotero/" file-full-name))
                                   (print file-type)))))))

(setq helm-source-bibtex
  '((name                                      . "BibTeX entries")
    (init                                      . helm-bibtex-init)
    (candidates                                . helm-bibtex-candidates)
    (filtered-candidate-transformer            . helm-bibtex-candidates-formatter)
    (action . (("Insert BibTeX key"            . helm-bibtex-insert-key)
               ("Open PDF or HTML attachment"  . helm-bibtex-open-file-zzs)
               ("Open PDF file (if present)"   . helm-bibtex-open-pdf)
               ("Open URL or DOI in browser"   . helm-bibtex-open-url-or-doi)
               ("Insert citation"              . helm-bibtex-insert-citation)
               ("Insert reference"             . helm-bibtex-insert-reference)
               ("Insert BibTeX entry"          . helm-bibtex-insert-bibtex)
               ("Attach PDF to email"          . helm-bibtex-add-PDF-attachment)
               ("Edit notes"                   . helm-bibtex-edit-notes)
               ("Show entry"                   . helm-bibtex-show-entry)))))


;;============================================================================================================
;;                                            cedet
;; ;;A Gentle introduction to CEDET        http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;; ;;使用Emacs 23.2内置的CEDET             http://emacser.com/built-in-cedet.htm
;;============================================================================================================
;; (require'cedet)
;; (global-ede-mode t) ;;开启EDE-mode
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   ;;global-semantic-idle-completions-mode
;;                                   ;;                                  global-semanticdb-minor-mode
;;                                   global-semantic-highlight-func-mode
;;                                   global-semantic-decoration-mode
;;                                   global-semantic-show-parser-state-mode
;;                                   global-semantic-idle-summary-mode
;;                                   ;;                                  global-semantic-idle-local-symbol-highlight-mode
;;                                   ;;                                  global-semantic-show-unmatched-syntax-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (semantic-add-system-include "/usr/include/Eigen/Eigen/" 'c++-mode)
;; 代码快速跳转
;; (require 'semantic/analyze/refs)

;; (defadvice push-mark (around semantic-mru-bookmark activate)
;;   "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
;; If `semantic-mru-bookmark-mode' is active, also push a tag onto
;; the mru bookmark stack."
;;   (semantic-mrub-push semantic-mru-bookmark-ring
;;                       (point)
;;                       'mark)
;;   ad-do-it)
;; ;; fast-jump-back
;; (defun semantic-ia-fast-jump-back ()
;;   (interactive)
;;   (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
;;       (error "Semantic Bookmark ring is currently empty"))
;;   (let* ((ring (oref semantic-mru-bookmark-ring ring))
;;          (alist (semantic-mrub-ring-to-assoc-list ring))
;;          (first (cdr (car alist))))
;;     (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
;;         (setq first (cdr (car (cdr alist)))))
;;     (semantic-mrub-switch-tags first)))
;; ;; Binding shortcuts to F12 and S-F12
;; (defun semantic-ia-fast-jump-or-back (&optional back)
;;   (interactive "P")
;;   (if back
;;       (semantic-ia-fast-jump-back)
;;     (semantic-ia-fast-jump (point))))
;; (define-key semantic-mode-map [f12] 'semantic-ia-fast-jump-or-back)
;; (define-key semantic-mode-map [S-f12] 'semantic-ia-fast-jump-back)

;; ;; Integration with imenu
;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)

;;============================================================================================================
;;                                            eshell clean screen
;;============================================================================================================
;;设置eshell 中输入clear命令清屏
(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear or DOS' cls"
  (interactive)
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))
;;============================================================================================================
;;                                            flx-ido
;;============================================================================================================
(require 'flx-ido)
(require 'ido-vertical-mode)
(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-use-virtual-buffers "auto")
(setq ido-work-directory-match-only nil)
;;disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)

;; =========================
;; Customize the Ido group to change the Ido functionality.
;; To modify the keybindings, use the ido-setup-hook.
(add-hook 'ido-setup-hook 'ido-my-keys)
(defun ido-my-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map "\C-n" 'ido-next-match)
 (define-key ido-completion-map "\C-p" 'ido-prev-match) 
 )

;;============================================================================================================
;;                                            smart-parens
;;============================================================================================================
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(require 'smartparens-config)

;; keybinding management
(define-key sp-keymap (kbd "C-c C-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-c C-b") 'sp-backward-sexp)
(define-key sp-keymap (kbd "C-c C-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-c C-p") 'sp-previous-sexp)
(define-key sp-keymap (kbd "C-c C-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-c C-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-c C-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-c C-e") 'sp-end-of-sexp)

;; (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
;; (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
;; (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

;; (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;; (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

;; (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
;; (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;; (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

;; (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
;; (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

;; (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
;; (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
;; (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
;; (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
;; (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
;; (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
;; (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
;; (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
;; (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil :bind "C-("))
;; 输入"{"后自动换行缩进
(defun my-open-block-c-mode (id action context)
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))
(sp-local-pair 'c-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
(sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-open-block-c-mode))
;;============================================================================================================
;;                                            Window-numbering
;;============================================================================================================
(require 'window-numbering);;Alt-[0-9]在子窗口间切换，注意不是在buffer间切换
(window-numbering-mode 1)

;;============================================================================================================
;;                                            tabbar-mode
;;============================================================================================================
;;left blank

;;============================================================================================================
;;                                            yasnippet
;;============================================================================================================
(require 'yasnippet)
(yas-global-mode 1)
;; 快捷键设置：取消yasnippet对tab的占用，全部改成M-tab
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-<tab>") 'yas-expand)
(define-key yas-keymap (kbd "<tab>") nil)
(define-key yas-keymap (kbd "M-<tab>") 'yas-next-field-or-maybe-expand)
;; 修正yas filed中嵌套有sp overlay（即嵌套括号或引号）时，yas-keymap被sp-keymap覆盖失效，从而M-tab恢复绑定到yas-expand而
;; 不能跳转到下一个field中的问题
(define-key sp-keymap (kbd "M-<tab>") 'yas-next-field-or-maybe-expand)
(setq yas-snippet-dirs "~/.emacs.d/snippets")
;;============================================================================================================
;;                                            smex
;;============================================================================================================
;;(require 'smex) ;; Not needed if you use package.el
(smex-initialize) ;; Can be omitted. This might cause a (minimal) delay
;; when Smex is auto-initialized on its first run.
(global-set-key (kbd "C-M-x") 'smex)
(global-set-key (kbd "C-M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
(require 'xcscope)
(cscope-setup)
(setq cscope-option-include-directories (quote ( "/usr/include/c++/4.8"
                                                 "/usr/include/x86_64-linux-gnu/c++/4.8"
                                                 "/usr/include/c++/4.8/backward"
                                                 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
                                                 "/usr/local/include"
                                                 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
                                                 "/usr/include/x86_64-linux-gnu"
                                                 "/usr/include"
                                                 )));;设置额外的搜索目录,由命令 echo "" | g++ -v -x c++ -E - 得到
;;============================================================================================================
;;                                            clang-format
;;============================================================================================================
;; 这是从apper下载的，所以在site-lisp下，如果在MELPA下载，就不用这句命令了
;; 关于格式化选项，参照：http://clang.llvm.org/docs/ClangFormatStyleOptions.html

(add-to-list 'load-path "/usr/share/emacs/site-lisp/clang-format-3.8")
(load "clang-format.el")
;;设置F10和C-F10为局部快捷键，只对c-mode有效
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key [f10] 'clang-format-buffer)
            (local-set-key [C-f10] 'clang-format-region)))

;;============================================================================================================
;;                                            flycheck
;;============================================================================================================
(add-hook 'after-init-hook #'global-flycheck-mode);;自动开启fly-check mode
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;;(setq flycheck-highlighting-mode (quote lines));;高亮模式，默认是symbols
(setq flycheck-clang-language-standard "c++11");;设置clang -std="c++11"
(setq flycheck-clang-include-path
      (quote ("/usr/local/include/Eigen")));;为clang添加include路径,其他需要的路径可以类似的在列表中添加
;;使用ycmd提供的checker,该checker只会在文件保存后才会启动检查，但是速度很快
(require 'flycheck-ycmd)

(flycheck-ycmd-setup)
(add-to-list 'flycheck-disabled-checkers 'c/c++-clang);;禁用自带的clang checker（慢）
(add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
(add-hook 'python-mode-hook (lambda () (add-to-list 'flycheck-disabled-checkers 'ycmd)))
;;用pop-up菜单显示出错信息,需要安装插件flycheck-pos-tip，该函数调用了flycheck中提供的flycheck-display-errors-function
;;变量来指定显示出错信息所调用的函数
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
(global-set-key [(control x) (up)] 'flycheck-previous-error)   ;;上一个错误
(global-set-key [(control x) (down)] 'flycheck-next-error)   ;;下一个错误
;;============================================================================================================
;;                                            AucTex&RefTex
;;============================================================================================================
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(require 'auctex-latexmk);;使auctex支持latexmk
(auctex-latexmk-setup)
(require 'reftex)
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
;;============================================================================================================
;;                                            popup-switcher
;;============================================================================================================
;; (require 'popup-switcher)
;; (setq psw-use-flx t)
;; (setq psw-in-window-center nil)
;; (global-set-key "\M-[" 'psw-switch-buffer)
;;============================================================================================================
;;                                            popwin
;;============================================================================================================
(require 'popwin)
(popwin-mode 1)
;;设置额外的窗口成为popwin
(push "*auto-async-byte-compile*" popwin:special-display-config)
(push "*cscope*" popwin:special-display-config)
(push "*Help*" popwin:special-display-config)
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
;;                                            cmake-mode
;;============================================================================================================
(require 'cmake-mode)


;;============================================================================================================
;;                                            multi-term
;;============================================================================================================
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (company-mode -1)))
(add-to-list 'term-bind-key-alist
             '("M-d" . term-send-forward-kill-word))
(multi-term-keystroke-setup)
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

;;============================================================================================================
;;                                            gnuplot-mode
;;============================================================================================================
(require 'gnuplot-mode)

;; 指定gunplot的路径 (如果不是/usr/bin/gnuplot的话)
(setq gnuplot-program "/usr/bin/gnuplot")

;; 打开后缀名为gp或plt的文件时自动开启gnuplot mode
(setq auto-mode-alist
      (append '(("\\.\\(gp\\|plt\\)$" . gnuplot-mode)) auto-mode-alist))
;; (add-hook 'gnuplot-mode-hook 'auto-complete-mode)
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

;;============================================================================================================
;;                                            pdf-tools
;;============================================================================================================
;;(pdf-tools-install)
;;============================================================================================================
;;                                            desktop
;;============================================================================================================
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq *desktop-dir* (list (expand-file-name "~/.emacs.d/desktops")))
(setq desktop-path '("~/.emacs.d/desktops"))
(setq desktop-dirname "~/.emacs.d/desktops")
(setq desktop-base-file-name ".emacs-desktop")
(desktop-read)


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
(global-set-key [(control x) (control a)] 'revert-buffer)
(global-set-key (kbd "C-\\") 'delete-whitespace-rectangle)
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


