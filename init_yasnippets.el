;;-----
;; yasnippets
;; - http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;;-----
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "vendors/my-snippets"))

(provide 'init_yasnippets)
