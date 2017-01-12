;;============================================================================================================
;;                                            popwin
;;============================================================================================================
(require 'popwin)
(popwin-mode 1)
;;设置额外的窗口成为popwin
(push "*auto-async-byte-compile*" popwin:special-display-config)
(push "*cscope*" popwin:special-display-config)
(push "*Help*" popwin:special-display-config)

(provide 'init-popwin)
;;; init-popwin.el ends here
