;;; package --- Summary

;;; Commentary:

;;; code:

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package) 
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))


;;设置python可执行程序的目录，ein和elpy采用的是anaconda自带的python，比较适合于科学计算
;; (setq python-executable-path "/opt/anaconda3/bin/")
(setq python-executable-path "")
;;设置el-get
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))


(unless (require 'el-get nil 'noerror)
  ;;只有el-get没有安装的时候才会执行函数体
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          ;; do not build recipes from emacswiki due to poor quality and documentation
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp)))  
;; 从melpa下载的软件包里有个pkg.el，会说明与其他包的依赖关系。但是问题是这个依赖关系包含版本号，所以
;; 当依赖关系中的某个包不是从melpa上下载的时候，就会报错说找不到package xx-0.0之类，所以现在只从melpa上下载软件包。
;; el-get包里自带的有一堆recipe，但是很多源都不是melpa的，经常出问题，所以每次安装el-get的时候执行el-get-elpa-build-local-recipes
;; 自动下载melpa的recipes到本地目录。似乎本地目录的优先级更高，可以保证软件包都从melpa上下载
  (el-get-elpa-build-local-recipes "~/.emacs.d/core/el-get-user-recipes/")
 )

(setq el-get-git-shallow-clone t)
(setq el-get-allow-insecure t)

;;自定义的初始化文件
(setq el-get-user-package-directory "~/.emacs.d/core/el-get-init-files/")
(add-to-list 'el-get-recipe-path "~/.emacs.d/core/el-get-user-recipes/")


;;用这个得到已安装的package列表
;; (dolist (package-name my-packages)
;;   (insert (concat package-name "\n")))

;package之间的顺序很重要，有些包可能依赖其他的包，可以考虑通过with-eval-after-load命令或者use-package包解决？

(setq required-packages
      '(pos-tip
       auctex
       auctex-latexmk
       auto-compile
       async
       bookmark+
       comment-dwim-2
       dash
       s
       f
       deferred
       request-deferred
       let-alist
       avy
       company-quickhelp
       company
       ycmd
       company-ycmd
       ein
       elpy
       find-file-in-project
       flx-ido
       flycheck       
       flycheck-pos-tip
       flycheck-ycmd
       fuzzy
       gnuplot-mode
       helm-core
       helm
       helm-ag
       helm-cscope
       helm-dash
       helm-gtags
       helm-bibtex
       helm-swoop
       ido-vertical-mode
       langtool
       multi-term
       org-bullets
       popup
       popwin
       rainbow-delimiters
       smartparens
       smex
       sqlup-mode
       swiper
       window-numbering
       vimish-fold
       spaceline
       xcscope
       yasnippet))

;;自动安装软件,如果有问题可以尝试删掉el-get目录下的.loaddefs.el文件
(el-get 'sync required-packages)



;;(dolist (this-package required-packages)
;;(el-get-bundle this-package))


;; (require 'package)
;; (package-initialize)

;;载入自己的配置文件
(add-to-list 'load-path "~/.emacs.d/core/")
(require 'basic)
(require 'keybindings)
(require 'functions)

;;开启emacs服务器功能
(server-start)
;;加载spaceline
(spaceline-spacemacs-theme)

;; (provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#002B36" "#E32368" "#859900" "#BDB35E" "#5BC2D6" "#E857DC" "#31DBC3" "#7A9496"])
 '(company-auto-complete nil)
 '(company-auto-complete-chars (quote ignore))
 '(company-backends
   (quote
    (company-capf company-ispell company-yasnippet company-files company-elisp company-css company-eclim company-semantic company-xcode company-ropemacs company-cmake company-bbdb
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev)))
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-preview-if-just-one-frontend company-quickhelp-frontend)))
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 999)
 '(company-require-match t)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 50)
 '(company-tooltip-offset-display (quote scrollbar))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("b25fdbe6a95053ca8a14f20d67f92573c8988765ef72c3b6a9097df20309de97" "bc8b846c01ec37e246ee8b12d6a406857a23dcd06df90e932aaf7c67111ca10a" default)))
 '(fci-rule-color "#232526")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-M-x-fuzzy-match t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*Messages")))
 '(helm-buffer-max-length 30)
 '(helm-buffers-fuzzy-matching t)
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
 '(helm-display-source-at-screen-top nil)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(helm-locate-fuzzy-match t)
 '(highlight-changes-colors (quote ("#E857DC" "#8D69CF")))
 '(highlight-tail-colors
   (quote
    (("#232526" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#232526" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-latex-default-packages-alist
   (quote
    (("" "fontspec" t)
     ("" "minted" t)
     ("UTF8, heading=true" "ctex" t)
     ("AUTO" "inputenc" t)
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
     ("" "hyperref" nil)
     "\\tolerance=1000")))
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#31DBC3" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#E32368" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#94C929"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#5BC2D6" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#BDB35E"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#E857DC"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#E88A1C" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#E32368")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#BDB35E")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#94C929")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#31DBC3")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#5BC2D6"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002B36" "#232526" "#A20C41" "#E32368" "#67930F" "#94C929" "#968B26" "#BDB35E" "#21889B" "#5BC2D6" "#A41F99" "#E857DC" "#349B8D" "#31DBC3" "#7A9496" "#BCBCBC"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
