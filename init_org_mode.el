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

(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                     ("NEXT" :foreground "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "yellow" :weight bold)
                                     ("SOMEDAY" :foreground "goldenrod" :weight bold)
                                     ("CANCELLED" :foreground "orangered" :weight bold)
                                     )))

(setq org-use-fast-todo-selection t)
(setq org-cycle-separator-lines 0)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("DONE" ("CANCELLED") ("WAITING")))))

(require 'remember)
(org-remember-insinuate)
(setq org-directory "~/Documents/org-files")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-remember-store-without-prompt t)
(setq org-remember-default-headline "Tasks")
;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates (quote (("todo" ?t "* TODO %?\n  %U\n  %a\n" "~/Documents/org-files/refile.org" bottom nil)
                                     ("note" ?n "* %?                                        :NOTE:\n  %U\n  %a\n" nil bottom nil)
                                     ("org-protocol" ?w "* TODO Review %c%!\n  %U" nil bottom nil))))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-agenda-custom-commands
      (quote (("w" "Tasks waiting on something" tags "WAITING/!"
               ((org-use-tag-inheritance nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-with-date nil)
                (org-agenda-overriding-header "Waiting Tasks")))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-overriding-header "Tasks to Refile")))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")))
              ("n" "Next" tags-todo "-WAITING-CANCELLED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")))
              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
               ((org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-overriding-header "Projects")))
              ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
               ((org-agenda-skip-function 'bh/skip-projects)
                (org-agenda-overriding-header "Other Non-Project Tasks")))
              ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
               ((org-agenda-overriding-header "Tasks to Archive")))
              ("h" "Habits" tags "STYLE=\"habit\""
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-overriding-header "Habits")))
              ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
               ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                (org-agenda-overriding-header "Stuck Projects")))
              ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
                (org-agenda-overriding-header "Set default clocking task with C-u C-u I"))))))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@InTown" . ?t)
                            ("@Work" . ?w)
                            ("@Home" . ?h)
                            (:endgroup)
                            ("WAITING" . ?W)
                            ("HOME" . ?H)
                            ("PLAY" . ?p)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?C))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Link Grabber setup
(add-to-list 'load-path (concat dotfiles-dir "vendors/org-mode/contrib/lisp"))
(require 'org-mac-link-grabber)

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
            (setq truncate-lines nil)
            (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)
            ))

;; Mobileorg setup
;; Set to the location of your Org files on your local system
(setq org-directory "~/Documents/org-files")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Documents/org-files/refile.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(provide 'init_org_mode)
