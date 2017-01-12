;; Capitalize keywords in SQL mode
(add-hook 'sql-mode-hook 'sqlup-mode)
;; Capitalize keywords in an interactive session (e.g. psql)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
;; Set a global keyword to use sqlup on a region

(add-hook 'sql-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)))


(provide 'init-sqlup-mode)
;;; init-flx-ido.el ends here
