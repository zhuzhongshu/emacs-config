(cscope-setup)
(setq cscope-option-include-directories (quote ( "/usr/include/c++/4.8"
                                                 "/usr/include/x86_64-linux-gnu/c++/4.8"
                                                 "/usr/include/c++/4.8/backward"
                                                 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
                                                 "/usr/local/include"
                                                 "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
                                                 "/usr/include/x86_64-linux-gnu"
                                                 "/usr/include"
                                                 )));;设置额外的搜索目录,由命令 echo "" | g++ -v -x c++ -E - 得到
(provide 'init-xcscope)
;;; init-xcscope.el ends here
