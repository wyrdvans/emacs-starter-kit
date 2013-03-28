;;;;;
;; Setup pymacs and rope
;;;;;
(add-to-list 'load-path (concat dotfiles-dir "vendors/pymacs"))
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t
;;       ropemacs-guess-project t
;;       )

;;;;;
;; Integrate Rope and yasnippet into auto-complete
;;;;;
;; (ac-ropemacs-setup)

;;;;;
;; setup pycomplexity
;;;;;
(add-to-list 'load-path (concat dotfiles-dir "vendors/pycomplexity/"))

(require 'linum)
(require 'flymake)
(require 'pycomplexity)

;;;;;
;; setup syntax/style/unittest checking
;;;;;
(add-to-list 'load-path (concat dotfiles-dir "vendors/flymake-python"))

(setq-default flymake-no-changes-timeout '3)
;; (setq-default flymake-log-level 3)
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (options (when trigger-type (list "--trigger-type" trigger-type))))
      (list (concat dotfiles-dir "vendors/flymake-python/pyflymake.py") (append options (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  )

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;;;;;
;; python-mode hook
;;;;;
(add-hook 'python-mode-hook
          (lambda ()
            (setq modelinepos-column-limit 79)
            (column-number-mode 1)
            (size-indication-mode 1)
            ;; autocomplete additions
            (auto-complete-mode 1)
            (add-to-list 'ac-sources 'ac-source-ropemacs)
            ;; pycomplexity additions
            ;; (pycomplexity-mode)
            (linum-mode)
            ;; lint checking additions
            (flymake-mode)
            ))

;; Set Python interpreter
;(setq python-python-command "ipython")
;(require 'ipython)

(provide 'init_python_mode)
