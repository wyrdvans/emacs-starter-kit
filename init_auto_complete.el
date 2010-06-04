;;-----
;; Auto Complete config
;;-----
(add-to-list 'load-path (concat dotfiles-dir "vendors/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "vendors/auto-complete/dict"))
(ac-config-default)

(provide 'init_auto_complete)
