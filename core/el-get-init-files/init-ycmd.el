;;ycmd目前不支持anaconda
(set-variable 'ycmd-server-command '("python"  "/home/simplex/.emacs.d/ycmd/ycmd/"))
;; (set-variable 'ycmd-server-command `(,(concat python-executable-path "python3") "/home/simplex/.emacs.d/ycmd/ycmd/"))
(set-variable 'ycmd-global-config  "/home/simplex/.emacs.d/ycmd/cpp/ycm/.ycm_extra_conf.py");;额外的配置
(set-variable 'ycmd-extra-conf-whitelist '( "/home/simplex/.emacs.d/ycmd/cpp/ycm"))
;; (setq ycmd-python-binary-path  (concat python-executable-path "python3"))
(setq ycmd-python-binary-path "python3")


(global-ycmd-mode)

(provide 'init-ycmd)
;;; init-ycmd.el ends here
