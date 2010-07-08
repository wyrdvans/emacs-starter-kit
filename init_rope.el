;;-----
;; Pymacs and Ropemacs Setup
;;-----
(add-to-list 'load-path (concat dotfiles-dir "vendors/pymacs"))
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-autoimport t)

(provide 'init_rope)
