;; Common configurations between users.
(add-to-list 'load-path (concat dotfiles-dir "vendors"))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;; Appearance
(add-to-list 'load-path (concat dotfiles-dir "vendors/color-theme"))
(require 'color-theme)
(setq color-theme-is-global t)
(load-file (concat dotfiles-dir "vendors/theme.el" ))
(my-color-theme)

(setq column-number-mode t)
(setq pop-up-windows nil)
(setq visible-bell nil)

;;----------------------------------------------------------------------
;; Custom functions, keybindings and aliases
;;----------------------------------------------------------------------

(message "Loading functions, keybindings and aliases...")
(defun tasks () (interactive) (find-file "~/Documents/Work/StatusFiles/tasks.org"))
(defun emacs () (interactive) (find-file "~/.emacs"))

;;-----
;; Keybindings
;;-----
(global-set-key [f5] 'tasks)
(global-set-key (kbd "<f6> a") (lambda ()
                                 (interactive)
                                 (find-file "~/.aliasrc")))
(global-set-key (kbd "<f6> e") (lambda ()
                                 (interactive)
                                 (find-file (concat dotfiles-dir "my-init.el"))))

(global-set-key (kbd "<f7> t") 'trac-wiki)
(global-set-key (kbd "<f7> l") 'linum-mode)

(global-set-key (kbd "C-<f7>") 'geben)
(global-set-key (kbd "C-<kp-delete>") 'kill-word)
;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'my-org-todo)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "<f7>") 'set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> i") (lambda ()
                                 (interactive)
                                 (info "~/git/org-mode/doc/org.info")))
(global-set-key (kbd "<f9> o") 'org-occur)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> u") (lambda ()
                                 (interactive)
                                 (untabify (point-min) (point-max))))
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'my-save-then-publish)
(global-set-key (kbd "C-M-r") 'org-remember)
;(global-set-key (kbd "<C-tab>") 'bury-buffer)

;;----------------------------------------------------------------------
;; System Specific settings
;;----------------------------------------------------------------------
;;-----
;; Mac Settings
;;-----
(defun mac-only-settings ()
  "Setup mac only settings."
   ;;; tabbar mode
   ;;; Resources:
   ;;; http://www.emacswiki.org/emacs/TabBarMode
   ;;; http://amitp.blogspot.com/2007/04/emacs-buffer-tabs.html
  (require 'tabbar)
  (set-face-attribute
   'tabbar-default-face nil :background "gray60")
  (set-face-attribute
   'tabbar-unselected-face nil
   :background "gray85" :foreground "gray30" :box nil)
  (set-face-attribute
   'tabbar-selected-face nil
   :background "#f2f2f6" :foreground "black" :box nil)
  (set-face-attribute
   'tabbar-button-face nil
   :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute
   'tabbar-separator-face nil :height 0.7)

  (tabbar-mode t)

  (normal-erase-is-backspace-mode t)
  (setq default-frame-alist (quote ((menu-bar-lines . 1) 
				    (background-color . "#000000") 
				    (background-mode . dark)
				    (border-color . "black")
				    (cursor-color . "#ff0000") 
				    (foreground-color . "#dbdbdb")
				    (mouse-color . "black")
				    (tool-bar-lines . 0)
				    (vertical-scroll-bars)
				    (height . 60)
				    (width . 120))))
  )

;;-----
;; Console Settings
;;-----
(defun console-only-settings ()
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (setq mac-option-modifier 'meta)
)

;; Load Appropriate Settings
(if (or (eq window-system 'mac) (eq window-system 'ns)) (mac-only-settings)
  (console-only-settings)
)

;;-----
;; Trac-wiki mode
;; -Trac-wiki file - http://www.meadowy.org/~gotoh/trac-wiki/trac-wiki.el
;; -XML-RPC file - http://cvs.savannah.gnu.org/viewvc/*checkout*/weblogger/lisp/xml-rpc.el?root=emacsweblogs
;; -Patch for xml-rpc.el on emacs23 - http://savannah.nongnu.org/bugs/download.php?file_id=17263
;;-----
(require 'trac-wiki)
(trac-wiki-define-project "aweber-trac" "https://trac/trac" "jasons")
(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)
;;; - start orgstruct minor mode when trac-wiki is loaded.
(add-hook 'trac-wiki-mode-hook 'turn-on-orgstruct)

;;----------------------------------------------------------------------
;; Custom-set-faces
;;----------------------------------------------------------------------
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1" :weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon" :weight bold))))
 '(font-lock-type-face ((t (:foreground "PaleGreen" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod" :weight bold)))))
