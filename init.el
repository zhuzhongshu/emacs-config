;;; package --- Summary

;;; Commentary:

;;; code:

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))



;;设置el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch
          ;; do not build recipes from emacswiki due to poor quality and
          ;; documentation
          el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; build melpa packages for el-get
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("gnu" . "http://elpa.gnu.org/packages/")))
  (el-get-elpa-build-local-recipes)
  (el-get-emacswiki-build-local-recipes))
(setq el-get-git-shallow-clone t)
(setq el-get-user-package-directory "~/.emacs.d/core/el-get-init-files/")
(require 'package)

;;用这个得到已安装的package列表
;; (dolist (package-name my-packages)
;;   (insert (concat package-name "\n")))


;package之间的顺序很重要，有些包可能依赖其他的包
(setq required-packages
      '(pos-tip
       auctex-latexmk
       auto-compile
       async
       comment-dwim-2
       dash
       let-alist
       avy
       company
       company-quickhelp
       ycmd
       company-ycmd
       ein
       elpy
       flx-ido
       flycheck
       flycheck-pos-tip
       flycheck-ycmd
       fuzzy
       gnuplot-mode
       helm
       helm-core
       helm-ag       
       helm-cscope
       helm-dash
       helm-gtags
       helm-bibtex
       helm-swoop
       ido-vertical-mode
       langtool
       multi-term
       popup
       popwin
       rainbow-delimiters
       reftex
       s
       smartparens
       smex
       sqlup-mode
       swiper
       window-numbering
       spaceline
       xcscope
       yasnippet))
(el-get 'sync required-packages)

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
