;;============================================================================================================
;;                                           langtool
;;============================================================================================================
(setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-2.6/languagetool-commandline.jar"
      langtool-mother-tongue "en"
      langtool-disabled-rules '("WHITESPACE_RULE"
                                "EN_UNPAIRED_BRACKETS"
                                "COMMA_PARENTHESIS_WHITESPACE"
                                "EN_QUOTES"))
(global-set-key (kbd "<f8>") 'langtool-check)           ;;首先摁F8开始语法检查，然后C-F8修正，最后M-F8退出
(global-set-key (kbd "C-<f8>") 'langtool-correct-buffer)
(global-set-key (kbd "M-<f8>") 'langtool-check-done)
(global-set-key (kbd "S-<f8>") 'langtool-show-message-at-point)
(global-set-key (kbd "C-S-<f8>") 'langtool-switch-default-language)
(provide 'init-langtool)
;;; init-langtool.el ends here
