;;;;;
;; Setup pymacs and rope
;;;;;
(add-to-list 'load-path (concat dotfiles-dir "vendors/pymacs"))
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t
      ropemacs-guess-project t
      )

;;;;;
;; Integrate Rope and yasnippet into auto-complete
;;;;;
(defun prefix-list-elements (list prefix)
   (let (value)
     (nreverse
      (dolist (element list value)
        (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

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
            (set (make-local-variable 'ac-dwim) nil)
            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
            (set (make-local-variable 'ac-candidates-function) 'ac-python-candidate)
            (set (make-local-variable 'ac-auto-start) 1)
            ;; pycomplexity additions
            (pycomplexity-mode)
            (linum-mode)
            ;; lint checking additions
            (flymake-mode)
            ))

(provide 'init_python_mode)
