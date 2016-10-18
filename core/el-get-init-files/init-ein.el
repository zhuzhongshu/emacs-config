;;============================================================================================================
;;                                            ein
;;============================================================================================================


;; (require 'ein)
;; (setq ein:console-executable "/usr/bin/ipython3")
(setq ein:console-executable
      (concat python-executable-path "ipython3"))
;; (setq ein:console-executable "/opt/anaconda3/bin/ipython3")

(provide 'init-ein)
;;; init-ein.el ends here
