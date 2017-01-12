(require 'yasnippet)
(require 'smartparens)
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

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
