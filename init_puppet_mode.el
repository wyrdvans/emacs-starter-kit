(add-to-list 'load-path (concat dotfiles-dir "vendors/puppet-flymake"))

(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")

(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(require 'flymake-puppet)

(add-hook 'puppet-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      'delete-trailing-whitespace nil t)
            (flymake-puppet-load)))
