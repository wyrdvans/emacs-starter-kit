;;-----
;; python-mode
;;-----
(add-hook 'python-mode-hook
          (lambda ()
            (setq modelinepos-column-limit 79)
            (column-number-mode 1)
            (size-indication-mode 1)
            ))

(provide 'init_python_mode)
