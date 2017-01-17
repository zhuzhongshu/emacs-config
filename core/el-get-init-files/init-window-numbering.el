(require 'window-numbering)
(window-numbering-mode 1)
(setq window-numbering-auto-assign-0-to-minibuffer nil)
;;设置alt-0在两个window之间切换
(define-key window-numbering-keymap "\M-0" 'other-window)
(provide 'init-window-numbering)
;;; init-window-numbering.el ends here
