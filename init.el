;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for loading files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "load a file in current user's configuration directory
   if it exists."
  (let ((fullfile (expand-file-name file user-init-dir)))
    (when (file-exists-p fullfile)
      (load-file fullfile))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packaged management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks nil)

;; Set coding font with ligature support
(custom-set-faces
 '(default ((t (:height 180 :family "Fira Code")))))

;; Clean up the modeline
(use-package delight
  :config
  (delight '((visual-line-mode)
	     (eldoc-mode nil "eldoc")
	     (yas-minor-mode nil "yasnippet")
	     (hs-minor-mode nil "hideshow")
	     (undo-tree-mode)
             (emacs-lisp-mode "λ" :major)
             (js-mode "JS" :major)
	     )))

;; Completely hide when we need focus
(use-package hide-mode-line)

;; Ivy: Generic completion framework
(use-package ivy
  :delight
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (bind-key "C-c C-r" 'ivy-resume)
  (ivy-mode 1))

;; Ivy enhanced version of Isearch
(use-package swiper
  :delight
  :config
  (global-set-key "\C-s" 'swiper))

;; Counsel: Collection of Ivy-enhanced common Emacs commands.
(use-package counsel
  :delight
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :custom
  (ivy-height 20 "number of result lines to display"))

;; Transform Ivy display items to have more information.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Navigate directly to specific windows quickly.
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Which Key: Discover what keys are associated with a keymap.
(use-package which-key
  :delight
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

 ;; Theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-Iosvkem t)
  )

;; Where to find secrets
(setq auth-sources '("~/.authinfo.gpg"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bt/set-initial-evil-states ()
  "Set the initial Evil states for various modes."
  (cl-loop for (mode . state)
	   in '((inferior-emacs-lisp-mode . emacs)
		(nrepl-mode . insert)
		(pylookup-mode . emacs)
		(comint-mode . normal)
		(shell-mode . insert)
		(git-commit-mode . insert)
		(git-rebase-mode . emacs)
		(term-mode . emacs)
		(help-mode . emacs)
		(image-mode . emacs)
		(deft-mode . emacs)
		(helm-grep-mode . emacs)
		(grep-mode . emacs)
		(bc-menu-mode . emacs)
		(magit-branch-manager-mode . emacs)
		(rdictcc-buffer-mode . emacs)
		(dired-mode . emacs)
		(elfeed-search . emacs)
		(wdired-mode . normal))
	   do (evil-set-initial-state mode state)))

;; Evil Package
(use-package evil
  :init
  (setq evil-want-C-i-jump nil) ;; Fix tab key in org-mode
  :config
  (evil-mode 1)
  (bt/set-initial-evil-states)
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-commentary
    :delight
    :init
    (evil-commentary-mode))
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UX Adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keybinding
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer bt/leader-key-def
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (bt/leader-key-def
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")

   "t" '(:ignore t :which-key "Toggle")
   "tl" '(toggle-truncate-lines :which-key "Truncate lines")
   "tw" '(whitespace-mode :which-key "Whitespace")

   ;; Org-mode
   "o" '(:ignore t :which-key "Org")
   "ob" '(org-switchb :which-key "Switch to Org Buffer")
   "o/" '(org-occur-in-agenda-files :which-key "Occur in Agenda files")
   "oj" '(:ignore t :which-key "Jump")
   "ojc" '(org-clock-goto :which-key "Current Clock")
   "ojh" '(counsel-org-agenda-headlines :which-key "Headline")
   "oa" '(org-agenda :which-key "Org agenda")
   "ot" '(:ignore t :which-key "Toggle")
   "oth" '(org-toggle-heading :which-key "Heading")
   "oti" '(org-toggle-item :which-key "Item")
   "otl" '(org-toggle-link-display :which-key "Link display")
   "ow" '(:ignore t :which-key "Web Tools")
   "owl" '(org-web-tools-insert-link-for-url :which-key "Insert Link")
   "owr" '(org-web-tools-read-url-as-org :which-key "Read URL")

   ;; Files
   "f" '(:ignore t :which-key "Files")
   "fr"  '(counsel-recentf :which-key "Recent Files")
   "fd"  '(find-dired :which-key "Find Files")

   ;; Buffers
   "b" '(:ignore t :which-key "Buffers")
   "bi"  '(ibuffer :which-key "iBuffer")
   "bb"  '(counsel-switch-buffer :which-key "Switch Buffer")

   ;; Others
   "a" '(:ignore t :which-key "Apps")
   "as" '(:ignore t :which-key "Shells")
   "ase"  '(eshell :which-key "Eshell")
   "ass"  '(counsel-switch-to-shell-buffer :which-key "Switch to Shell")
   "ar"  '(bt/restclient-sandbox :which-key "Restclient Sandbox")

   ;; Magit
   "g" '(:ignore t :which-key "Magit")
   "gs" '(magit-status :which-key "Status")
   "gd" '(magit-diff-unstaged :which-key "Diff Unstaged")
   "gl" '(:ignore t :which-key "Log")
   "glc" '(magit-log-current :which-key "Current Branch")
   "gla" '(magit-log-all :which-key "All")
   "glf" '(magit-log-buffer-file :which-key "Current File")

   ;; Writing
   "w" '(:ignore t :which-key "Writing")
   "wg" '(:ignore t :which-key "Writegood")
   "wgm" '(writegood-mode :which-key "Toggle Writegood Mode")
   "wgr" '(writegood-reading-ease :which-key "Reading Ease Score")
   "wr" '(:ignore t :which-key "Writeroom")
   "wrm" '(writeroom-mode :which-key "Toggle Writeroom")
   "wc" '(:ignore t :which-key "Word Count")
   "wcm" '(wc-mode :which-key "Toggle Wordcount mode")
   "wcg" '(wc-set-word-goal :which-key "Set Word Goal")
   "wcr" '(wc-reset :which-key "Reset Word Goal")
   "wd" '(:ignore t :which-key "Define")
   "wdp" '(define-word-at-point :which-key "At Point")
   "wdd" '(define-word :which-key "Define Word")
   "wf" '(:ignore :which-key "Focus")
   "wff" '(focus-mode :which-key "Focus Mode")
   "wfc" '(focus-change-thing :which-key "Change")
   ))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Better undo
(use-package undo-fu
  :config
  ;; (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; Company for autocomplete
(use-package company
  :delight
  :config
    (add-hook 'after-init-hook 'global-company-mode)
  :hook (prog-mode . company-mode)
  :init
    (global-set-key (kbd "C-x c") 'company-complete-common)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

;; Dired
(when (eq system-type 'darwin)
  ;; installed with `brew install coreutils`
  ;; use proper GNU ls
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package dired+
    :init
    (progn
	;; Details toggling is bound to "(" in `dired-mode' by default
	(setq diredp-hide-details-initially-flag t)
	(setq dired-listing-switches "-aghBo --group-directories-first")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "h" 'dired-hide-dotfiles-mode))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Projectile
(use-package projectile
  :delight
  :init
  ;; Make Projectile usable even outside of project roots
  (setq projectile-require-project-root nil)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

;; Snippets
(use-package yasnippet
  :delight
  :defer t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :defer t)

;; Make align-regex insert spaces instead of tabs
;;     See: https://stackoverflow.com/a/25164056
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Evil-Magit
(use-package evil-magit
  :after evil magit)

;; Interact with Issues from places like Github
(use-package forge
  :after magit)

;; Ledger for accounting
(use-package ledger-mode
  :defer t)

;; News and Reading
(use-package pocket-reader
  :defer t
  :init
    (evil-set-initial-state 'pocket-reader-mode 'emacs))

;; Elfeed News
(use-package elfeed-org
  :defer t
  :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Documents/personal/rss/rss-feeds.org")))

(defun pp/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

(defun pp/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer."
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :defer t
  :init
    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (evil-set-initial-state 'elfeed-show-mode 'emacs)
    (setq elfeed-db-directory "~/Sync/personal/rss/.elfeed")
  :bind
    (:map elfeed-search-mode-map
              ("q" . pp/elfeed-save-db-and-bury)))

(use-package elfeed-score
  :defer t
  :ensure t
  :init
    (setq elfeed-score-score-file "~/Sync/personal/rss/elfeed.score")
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.
The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file. A datetree within a subheading
is possible if the heading has a property of DATE_TREE."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
	(widen)
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree (+ org-datetree-base-level 3))
        (widen)
        ))))

(defun bt/set-org-agenda-files ()
  "Generates the list of Org agenda files."
  (interactive)
  (setq org-agenda-files
	(apply 'append (mapcar (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("~/Sync/personal/"
				 "~/Private/"
				 "~/Sync/learning/"
				 "~/Sync/fw/"
				 ))))
  ;; Ensure that the general org inbox is part of our agenda
  (push '"~/Sync/org/inbox.org" org-agenda-files))

(use-package org
  :init
    ; General Settings
    (setq org-hide-emphasis-markers t
	  org-clock-clocked-in-display nil
	  org-tags-column 0 ; tags are right after headline
	  org-hide-leading-stars t
	  org-adapt-indentation t
	  org-confirm-babel-evaluate nil
	  org-log-into-drawer t
	  org-log-done 'time
	  org-html-validation-link nil
	  org-export-with-section-numbers nil
	  org-export-with-toc nil
	  org-export-with-author nil
	  org-export-with-email nil
	  org-export-with-date nil
	  org-priority-default 67 ;; Have the default priority be "C"
	  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@)"))
          org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock){:} %TIMESTAMP %SCHEDULED(Scheduled) %DEADLINE(Deadline)"
          org-global-properties '(("Effort_ALL" . "0 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))
	  org-capture-templates '(("i" "Inbox Task" entry (file+headline "~/Sync/org/inbox.org" "Tasks")
				   "* TODO %?\n  %i\n  %a")))

    ; Agenda Settings
    (setq org-agenda-span 'day
	   org-agenda-todo-ignore-scheduled 'all
	   org-agenda-entry-text-maxlines 10
	   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :fileskip0 t :tags nil) ;; Clocktable in agenda view
	   org-agenda-skip-additional-timestamps-same-entry t
	   org-agenda-skip-scheduled-if-done t
	   org-agenda-skip-deadline-if-done t
	   org-agenda-sorting-strategy '((agenda time-up todo-state-down priority-down)
					 (todo priority-down category-up)
					 (tags priority-down category-keep)
					 (search category-keep))
	   org-agenda-use-time-grid t
	   org-agenda-time-grid '((daily today)
				  (800 1000 1200 1400 1600 1800 2000)
				  "......" "----------------"))

    ;; Improve org-refile across files
    ;;
    ;;   See: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
    ;;
    (setq org-refile-targets '((org-agenda-files :maxlevel . 5))
          org-goto-interface 'outline-path-completion
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm)

    ;; Make jumping to Org file headings fuzzy searchable using org-goto
    ;;
    ;;  See: https://emacs.stackexchange.com/questions/32617/how-to-jump-directly-to-an-org-headline
    ;;
    (setq org-goto-interface 'outline-path-completion
          org-outline-path-complete-in-steps nil)

  :config
    (global-set-key (kbd "C-c o a") 'org-agenda)
    (global-set-key (kbd "C-c o c") 'org-capture)
    (global-set-key (kbd "C-c o b") 'org-switchb)
    (global-set-key (kbd "C-c o r d") 'org-refile-to-datetree)

    ;; View item from agenda in narrowed buffer.
    ;; Useful when using "follow" in agenda views.
    ;;     See: https://emacs.stackexchange.com/questions/17797/how-to-narrow-to-subtree-in-org-agenda-follow-mode
    ;;
    (advice-add 'org-agenda-goto :after
		(lambda (&rest args)
		  (org-narrow-to-subtree)))

    ;; Define babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
        (shell . t)
	(sql . t)
	(python . t)
	(css . t)
	(js . t)
       ))

    (load-user-file ".org-capture-templates.el")

    (bt/set-org-agenda-files)

) ;; End orgmode config

(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :config
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

(use-package org-habit-plus
  :defer t
  :straight (:host github :repo "myshevchuk/org-habit-plus")
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 50
      org-habit-preceding-days 30
      org-habit-show-habits-only-for-today nil
      org-habit-show-done-always-green t))

(require 'org-tempo) ; needed to make <s<tag> expand to src blocks in macos

;; Additional Orgmode Packages
(eval-after-load "org"
  '(require 'ox-md nil t))

(use-package ox-twbs)

(use-package ox-reveal
  :straight (:host github :repo "yjwen/org-reveal"))

(use-package org-journal
  :defer t
  :bind
    ("C-c n j" . org-journal-new-entry)
  :custom
    (org-journal-dir "~/Sync/org/roam/journal/")
    (org-journal-file-format "journal-%Y-%m-%d.org")
    (org-journal-date-prefix "#+CATEGORY: journal\n#+TITLE: Journal - ")
    (org-journal-date-format "%Y-%m-%d - %A"))

(defun org-journal-find-location ()
  "Open today's journal. Specify a non-nil prefix in order to inhibit
inserting the heading which will be handled by 'org-capture'."
    (org-journal-new-entry t)
    (goto-char (point-max)))

(add-to-list 'org-capture-templates
	     '("j" "Journal" plain (function org-journal-find-location)
	       "* %^{Title}\n%U\n%i\n%? "
	       :empty-lines 1
	       :jump-to-captured t))

(use-package org-roam
      :init
      (setq org-roam-v2-ack t)
      :ensure t
      :custom
      (org-roam-directory (file-truename "~/Sync/org/roam/"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture))
      :config
      (org-roam-db-autosync-mode)
      ;; If using org-roam-protocol
      ;; (require 'org-roam-protocol)
      )

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package writeroom-mode
  :defer t
  :config
    (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width))

(use-package focus
  :defer t)

(use-package wc-mode
  :defer t
  :init
    (setq wc-modeline-format "WC[%W%w/%tw:%gw]"))

(use-package define-word
  :defer t
  :straight (:host github :repo "abo-abo/define-word"))

(use-package clear-text
  :defer t
  :straight (:host github :repo "xuchunyang/clear-text.el"))

(use-package flyspell
  :defer t
  :delight
  :init
    (defun flyspell-check-next-highlighted-word ()
	"Custom function to spell check next highlighted word"
	(interactive)
	(flyspell-goto-next-error)
	(ispell-word))
  :config
    (global-set-key (kbd "C-c s m") 'flyspell-mode)
    (global-set-key (kbd "C-c s b") 'flyspell-buffer)
    (global-set-key (kbd "C-c s w") 'ispell-word)
    (global-set-key (kbd "C-c s c") 'flyspell-check-next-highlighted-word)
    (when (executable-find "hunspell")
	(setq-default ispell-program-name "hunspell")
	(setq ispell-really-hunspell t))
  :hook
    (text-mode . flyspell-mode))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)))

;; Edit Chrome textareas in Emacs
(use-package edit-server
  :init
    (setq edit-server-new-frame nil) ;; Open in new buffer of existing frame
  ;; Start the server and use markdown by default
  :config
    (edit-server-start)
    (add-hook 'edit-server-start-hook 'markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request Clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package restclient
  :defer t)

(use-package graphql-mode
  :defer t)

(use-package graphql
  :defer t)

(defun bt/restclient-sandbox ()
    "Create a new restclient sandbox to explore urls."
  (interactive)
  (let ((buffer (generate-new-buffer "restclient-sandbox.http")))
    (with-current-buffer buffer
      (restclient-mode)
      (insert "# restclient sandbox\n\n#\nHEAD https://example.com"))
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; Viewing Logs
(use-package logview
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier
;;   Requires: npm install -g prettier
(use-package prettier)

;; View Gherkin files
(use-package feature-mode)

;; Get POSIX shell everywhere
(use-package vterm)

;; Code Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Inherit shell environment
(use-package exec-path-from-shell
   :config (exec-path-from-shell-initialize))

(use-package ripgrep)

(use-package flycheck
  :delight
  :defer t)

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package lsp-mode
 :config
   (lsp-enable-which-key-integration t)
 :hook
   (php-mode . lsp)
   ;; (python-mode . lsp)
   ;; (sh-mode . lsp)
 :init
   (setq lsp-keymap-prefix "C-c l")
 :commands
   (lsp lsp-deferred))

(use-package lsp-ui)

(use-package lsp-ivy)

(use-package company-lsp
    :commands company-lsp)

;; Web Mode
(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :bind
  (:map web-mode-map
	("C-c C-e h" . web-mode-element-sibling-previous)
	("C-c C-e l" . web-mode-element-sibling-next)))

;; Emmet
(use-package emmet
  :straight (:host github :repo "joostkremers/emmet-for-emacs")
  :defer t
  :delight (emmet)
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install individual language configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PHP
(use-package php-mode
  :defer t
  :hook (php-mode . lsp-deferred))

;; JS
(use-package typescript-mode
  :defer t
  :delight
  (typescript-mode "TS" :major)
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package ob-typescript)

(use-package json-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2    ; and some old ones, too
      auto-save-interval 20)
;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
	auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
	auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
	tramp-backup-directory-alist `((".*" . ,backup-dir))
	tramp-auto-save-directory auto-saves-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Separate Customization file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((custom-filename ".custom.el"))
  (when (file-exists-p custom-filename)
    (setq custom-file (expand-file-name custom-filename user-init-dir))
    (load custom-file)
  ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
