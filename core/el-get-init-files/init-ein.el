;;============================================================================================================
;;                                            ein
;;============================================================================================================
(defvar python-executable-path);;仅仅是为了防止出现 assign to free variable的警告...
(setq python-executable-path "/opt/anaconda3/bin/");;python可执行程序的目录，ein和elpy采用的是anaconda自带的python，比较适合于科学计算
;; (require 'ein)
;; (setq ein:console-executable "/usr/bin/ipython3")
(setq ein:console-executable
      (concat python-executable-path "ipython3"))
;; (setq ein:console-executable "/opt/anaconda3/bin/ipython3")

(provide 'init-ein)
;;; init-ein.el ends here
