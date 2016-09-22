(avy-setup-default)
(setq avy-background t)
(setq avy-keys (number-sequence ?a ?z))
;; (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))     
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c SPC") 'avy-goto-char)
(global-set-key (kbd "M-g M-g") 'avy-goto-line)
(global-set-key (kbd "C-c w") 'avy-goto-word-1)

(provide 'init-avy)
;;; init-avy.el ends here
