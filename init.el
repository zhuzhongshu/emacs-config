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
      '(spaceline
        pos-tip
        auctex
       auctex-latexmk
       auto-compile
       async
       bookmark+
       comment-dwim-2
       ;;chinese-fonts-setup
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

