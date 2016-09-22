;;============================================================================================================
;;                                            vimish fold
;;============================================================================================================
(package-installed-p 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-c f") 'vimish-fold)

(provide 'init-vimish-fold)
;;; init-vimish-fold.el ends here
