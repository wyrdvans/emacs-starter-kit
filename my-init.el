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
(setq ns-pop-up-frames nil)
(setq visible-bell nil)

;;----------------------------------------------------------------------
;; Custom functions, keybindings and aliases
;;----------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.alias\\(rc\\|es\\)$" . sh-mode))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'log-edit-mode-hook (lambda () (auto-fill-mode nil)))

;;-----
;; Keybindings
;;-----
(global-set-key (kbd "<f5>") 'my-org-todo)
(global-set-key (kbd "<S-f5>") 'widen)

(global-set-key (kbd "<f6> a") (lambda ()
                                 (interactive)
                                 (find-file "~/.aliasrc")))
(global-set-key (kbd "<f6> e") (lambda ()
                                 (interactive)
                                 (find-file (concat dotfiles-dir "my-init.el"))))
(global-set-key (kbd "<f6> z e") (lambda ()
                                   (interactive)
                                   (find-file "~/.zshenv")))
(global-set-key (kbd "<f6> z r") (lambda ()
                                   (interactive)
                                   (find-file "~/.zshrc")))

(global-set-key (kbd "<f7> l") 'linum-mode)
(global-set-key (kbd "<f7> g") 'geben)
(global-set-key (kbd "<f7> t") 'trac-wiki)
(global-set-key (kbd "<f7> w") 'set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;; (global-set-key (kbd "<f9> b") 'bbdb)
;; (global-set-key (kbd "<f9> c") 'calendar)
;; (global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;; (global-set-key (kbd "<f9> g") 'gnus)
;; (global-set-key (kbd "<f9> i") (lambda ()
;;                                  (interactive)
;;                                  (info "~/git/org-mode/doc/org.info")))
;; (global-set-key (kbd "<f9> o") 'org-occur)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> u") (lambda ()
                                 (interactive)
                                 (untabify (point-min) (point-max))))
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> y") (lambda ()
                                 (interactive)
                                 (yas/load-directory (concat dotfiles-dir "vendors/my-snippets"))))
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "<f12>") 'org-agenda)
;; (global-set-key (kbd "C-s-<f12>") 'my-save-then-publish)

(global-set-key (kbd "C-<kp-delete>") 'kill-word)
(global-set-key (kbd "C-M-r") 'org-remember)
(global-set-key (kbd "C-x n r") 'narrow-to-region)


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

  (add-to-list 'exec-path "/opt/local/bin")
  (setq ispell-program-name "aspell")
  (setenv "ASPELL_CONF" nil)

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
(trac-wiki-define-project "aweber-trac" "https://trac.colo.lair/trac" "jasons")
(autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)
;;; - start orgstruct minor mode when trac-wiki is loaded.
(add-hook 'trac-wiki-mode-hook (lambda ()
                                 (turn-on-orgstruct)
                                 (auto-fill-mode 0)))

;;-----
;; Org mode
;; -http://orgmode.org
;;-----
(add-to-list 'load-path (concat dotfiles-dir "vendors/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "PROJECT(P@)" "OPEN(O@)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                     ("STARTED" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("SOMEDAY" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("QUOTE" :foreground "red" :weight bold)
                                     ("QUOTED" :foreground "magenta" :weight bold)
                                     ("APPROVED" :foreground "forest green" :weight bold)
                                     ("EXPIRED" :foreground "forest green" :weight bold)
                                     ("REJECTED" :foreground "forest green" :weight bold)
                                     ("OPEN" :foreground "blue" :weight bold)
                                     ("PROJECT" :foreground "red" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-cycle-separator-lines 0)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("STARTED" ("WAITING"))
              ("PROJECT" ("CANCELLED") ("PROJECT" . t)))))

(require 'remember)
(org-remember-insinuate)

(setq org-remember-store-without-prompt t)
(setq org-remember-default-headline "Tasks")
;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates (quote (("todo" ?t "* TODO %?
  %u
  %a" "~/Documents/org-files/tasks.org" bottom nil)
                                     ("note" ?n "* %?                                        :NOTE:
  %u
  %a" nil bottom nil)
                                     ("phone" ?p "* PHONE %:name - %:company -                :PHONE:
  Contact Info: %a
  %u
  :CLOCK-IN:
  %?" "~/Documents/org-files/phone.org" bottom nil))))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-agenda-custom-commands
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("n" "Notes" tags "NOTES" nil))))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@InTown" . ?t)
                            ("@Work" . ?w)
                            ("@Home" . ?h)
                            (:endgroup)
                            ("NEXT" . ?N)
                            ("PROJECT" . ?P)
                            ("WAITING" . ?W)
                            ("HOME" . ?H)
                            ("PLAY" . ?p)
                            ("CANCELLED" . ?C))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; flyspell mode to spell check everywhere
            (flyspell-mode 1)
            (auto-fill-mode 0)
            ))


;;-----
;; Periodic commands
;;-----
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;;-----
;; yasnippets
;; - http://yasnippet.googlecode.com/svn/trunk/doc/index.html
;;-----
(add-to-list 'load-path (concat dotfiles-dir "vendors/yasnippets"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "vendors/yasnippets/snippets"))
(yas/load-directory (concat dotfiles-dir "vendors/my-snippets"))

;;-----
;; php-mode
;; - http://php-mode.sourceforge.net/
;;-----
(require 'php-mode)

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
