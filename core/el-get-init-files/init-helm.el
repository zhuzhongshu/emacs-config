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


(provide 'init-helm)
;;; init-helm.el ends here
