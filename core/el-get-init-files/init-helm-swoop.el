(setq helm-swoop-pre-input-function
      (lambda () ""))
(global-set-key (kbd "C-r") 'helm-ag)
(global-set-key (kbd "M-x") 'helm-M-x);;大多数情况下helm-M-x可以工作的很好，但是它不支持用C-u传入参数，此时要用smex，绑定到了C-M-x
(global-set-key "\M-[" 'helm-buffers-list);;用helm切换buffer
(global-set-key "\M-]" 'helm-register);;用helm切换buffer
(global-set-key "\M-y" 'helm-show-kill-ring);;C-x b的另一个绑定
(define-key global-map (kbd "C-c C-r") 'helm-resume)

(provide 'init-helm-swoop)
;;; init-helm-swoop.el ends here
