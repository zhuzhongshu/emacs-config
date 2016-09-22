(setq ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-virtual-buffers "auto")
(setq ido-work-directory-match-only nil)
;;disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)
(add-hook 'ido-setup-hook 'ido-my-keys)
(defun ido-my-keys ()
 "Add my keybindings for ido."
 (define-key ido-completion-map "\C-n" 'ido-next-match)
 (define-key ido-completion-map "\C-p" 'ido-prev-match))

(provide 'init-flx-ido)
;;; init-flx-ido.el ends here
