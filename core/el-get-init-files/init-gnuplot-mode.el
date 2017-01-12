;;============================================================================================================
;;                                            gnuplot-mode
;;============================================================================================================
;; 指定gunplot的路径 (如果不是/usr/bin/gnuplot的话)
(setq gnuplot-program "/usr/bin/gnuplot")

;; 打开后缀名为gp或plt的文件时自动开启gnuplot mode
(setq auto-mode-alist
      (append '(("\\.\\(gp\\|plt\\)$" . gnuplot-mode)) auto-mode-alist))

(provide 'init-gnuplot-mode)
;;; init-gnuplot-mode.el ends here
