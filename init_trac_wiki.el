;;-----
;; Trac-wiki mode
;; -Trac-wiki file - http://www.meadowy.org/~gotoh/trac-wiki/trac-wiki.el
;; -XML-RPC file - http://cvs.savannah.gnu.org/viewvc/*checkout*/weblogger/lisp/xml-rpc.el?root=emacsweblogs
;; -Patch for xml-rpc.el on emacs23 - http://savannah.nongnu.org/bugs/download.php?file_id=17263
;;-----
(require 'trac-wiki)
(trac-wiki-define-project "aweber-trac" "https://trac.colo.lair/trac" "jasons")
(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)
;;; - start orgstruct minor mode when trac-wiki is loaded.
(add-hook 'trac-wiki-mode-hook (lambda ()
                                 (auto-fill-mode 0)))

(provide 'init_trac_wiki)
