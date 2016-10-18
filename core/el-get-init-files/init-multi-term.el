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
(provide 'init-multi-term)
;;; init-multi-term.el ends here
