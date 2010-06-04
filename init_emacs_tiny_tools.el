;;-----
;; Emacs-tiny-tools
;; ----
(add-to-list 'load-path (concat dotfiles-dir "vendors/emacs-tiny-tools/lisp/tiny"))
(add-to-list 'load-path (concat dotfiles-dir "vendors/emacs-tiny-tools/lisp/other"))
(require 'tinyprocmail)
(autoload 'tinyprocmail-mode "tinyprocmail" "" t)
(autoload 'turn-on-tinyprocmail-mode "tinyprocmail" "" t)
(add-to-list 'auto-mode-alist '("\\.procmailrc\\|pm-.*\\.rc$") 'turn-on-tinyprocmail-mode)

(provide 'init_emacs_tiny_tools)
