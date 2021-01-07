(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

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
(straight-use-package 'org-plus-contrib)
(setq straight-use-package-by-default t)

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (menu-bar-mode -1)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks nil)

(setq display-time-format "%F %R")
(setq display-time-default-load-average nil)
(display-time-mode 1)

(global-set-key (kbd "C-x t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x t w") 'whitespace-mode)


;; Prefer splitting the window to the right
(setq split-height-threshold nil)
(setq split-width-threshold 200)

;; Clean up the modeline
(use-package diminish
  :config
    (diminish 'undo-tree-mode)
    (diminish 'visual-line-mode)
    )

(use-package ivy
  :diminish
  :demand
  :config
    (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
    (bind-key "C-c C-r" 'ivy-resume)
    (ivy-mode 1)
    )

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package swiper
  :diminish
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :diminish
  :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c a") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :custom
    (ivy-height 20 "number of result lines to display")
  )

;; Company for autocomplete
(use-package company
  :config
    (add-hook 'after-init-hook 'global-company-mode)
  :hook (prog-mode . company-mode)
  :init
    (global-set-key (kbd "C-x c") 'company-complete-common)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

;; Vim mode
(use-package evil
  :config
  (evil-mode 1)
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-commentary
    :diminish
    :init
    (evil-commentary-mode))
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1)))

(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
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
    do (evil-set-initial-state mode state))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Evil-Magit
(use-package evil-magit
  :after evil magit)

;; Which Key
(use-package which-key
  :diminish
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Custom keybinding
(use-package general
  :config (general-define-key
	   :states
	   '(normal visual insert emacs)
	   :prefix "SPC"
	   :non-normal-prefix "M-SPC"
	   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
	   "SPC" '(counsel-M-x :which-key "M-x")
	   ;; Projectile
	   "pp"   '(projectile-switch-project :which-key "Switch project")
	   "pd"   '(projectile-dired-other-window :which-key "Open dired in new window")
	   "pff"  '(projectile-find-file :which-key "Find files in project")
	   "pfd"  '(projectile-find-dir-other-window :which-key "Open project dired in other window")
	   ;; Org-mode
	   "oj" '(counsel-org-agenda-headlines :which-key "Org Jump to Headline")
	   "oa" '(org-agenda :which-key "Org agenda")
	   ;; Files
	   "fr"  '(counsel-recentf :which-key "Recent Files")
	   ;; Buffers
	   "bb"  '(counsel-switch-buffer :which-key "Switch Buffer")
	   "bd"  '(evil-delete-buffer :which-key "Delete Buffer")
	   "bn"  '(evil-next-buffer :which-key "Next Buffer")
	   "bp"  '(evil-prev-buffer :which-key "Next Buffer")
	   "bN"  '(evil-buffer-new :which-key "New Buffer")
	   ;; Window
	   "wl"  '(windmove-right :which-key "move right")
	   "wh"  '(windmove-left :which-key "move left")
	   "wk"  '(windmove-up :which-key "move up")
	   "wj"  '(windmove-down :which-key "move bottom")
	   "w/"  '(split-window-right :which-key "split right")
	   "w-"  '(split-window-below :which-key "split bottom")
	   "wx"  '(delete-window :which-key "delete window")
	   ;; Others
	   "at"  '(ansi-term :which-key "open terminal")
	   ))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Increase the number of recent files that are stored
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 50)

;; Powerline

(use-package powerline
  :straight (:host github :repo " milkypostman/powerline")
  :config
    (require 'powerline)
    (powerline-center-evil-theme))

;; Projectile
(use-package projectile
  :init
    (setq projectile-require-project-root nil)
  :config
    (projectile-mode 1)
    (setq projectile-completion-system 'ivy)
  )

;; Ledger for accounting
(use-package ledger-mode)


;; News and Reading
(use-package pocket-reader
  :init
    (evil-set-initial-state 'pocket-reader-mode 'emacs)
  )

;; Elfeed for News
;; use an org file to organise feeds
(use-package elfeed-org
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
  :init
    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (evil-set-initial-state 'elfeed-show-mode 'emacs)
    (setq elfeed-db-directory "~/Documents/personal/rss/.elfeed")
  :bind
    (:map elfeed-search-mode-map
              ("q" . pp/elfeed-save-db-and-bury)))

(use-package elfeed-score
  :ensure t
  :init
    (setq elfeed-score-score-file "~/Documents/personal/rss/elfeed.score")
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

;; Writing
(use-package writeroom-mode
  :config
    (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") #'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))

(use-package flyspell
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

(use-package writegood-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)))

;; OrgMode Configs
(use-package org
  :init
    ; General Settings
    (setq org-hide-emphasis-markers t
	  org-confirm-babel-evaluate nil
	  org-log-into-drawer t
	  org-return-follows-link t
	  org-log-done 'time
	  org-html-validation-link nil
	  org-export-with-section-numbers nil
	  org-priority-default 67 ;; Have the default priority be "C"
	  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@)"))
          org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock){:} %TIMESTAMP %SCHEDULED(Scheduled) %DEADLINE(Deadline)"
          org-global-properties '(
				  ("Effort_ALL" . "0 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")
				  ))
    ; Agenda Settings
     (setq org-agenda-span 'day
	   org-agenda-todo-ignore-scheduled 'all
	   org-agenda-entry-text-maxlines 10
           org-agenda-custom-commands
     	  '(("c" . "Custom Agenda Views")
	    ("cn" todo "NEXT")
     	    ("cp" "Planning, Fourteen day agenda with all unscheduled todos"
     	     ((agenda "" ((org-agenda-span 14)
     			  (org-agenda-start-on-weekday 0)))
     	      (alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))
	    ("cl" "Log View"
	     ((agenda "" ((org-agenda-start-with-log-mode '(closed clock state))
			  (org-agenda-archives-mode t)))))))

     ;; What agenda files are used?
     (setq org-agenda-files (apply 'append
				   (mapcar
				    (lambda (directory)
				      (directory-files-recursively
				       directory org-agenda-file-regexp))
				    '("~/Sync/personal/"
				      "~/Sync/fw/"
				      ))))

     (setq org-agenda-skip-additional-timestamps-same-entry t
	   org-agenda-skip-scheduled-if-done t
	   org-agenda-skip-deadline-if-done t
	   org-agenda-use-time-grid t
	   org-agenda-time-grid '((daily today)
				  (800 1000 1200 1400 1600 1800 2000)
				  "......" "----------------"))

     (setq org-agenda-sorting-strategy '((agenda time-up todo-state-down priority-down)
					 (todo priority-down category-up)
					 (tags priority-down category-keep)
					 (search category-keep)))

    ;; Improve org-refile across files
    ;;
    ;;   See: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
    ;;
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
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
    (global-set-key (kbd "C-c o l") 'org-store-link)
    (global-set-key (kbd "C-c o a") 'org-agenda)
    (global-set-key (kbd "C-c o c") 'org-capture)
    (global-set-key (kbd "C-c o b") 'org-switchb)
    (global-set-key (kbd "C-c o t j") 'org-clock-goto)
    (global-set-key (kbd "C-c o r d") 'org-refile-to-datetree)
    (global-set-key (kbd "C-c o u r") 'org-web-tools--read-url)

    (require 'org-tempo) ; needed to make <s<tag> expand to src blocks in macos

    ;; Helper to generate unique ids
    (require 'org-id)
    (setq org-id-link-to-org-use-id 'nil)
    (setq org-id-prefix (concat "org_" (user-real-login-name) "_" (format-time-string "%Y-%m-%d") "_" (system-name)))
    (setq org-id-method 'uuid)
    (defun help/org-id-new ()
      "Re-purposing `org-id' hit a snag when colons were forbidden in Source-Block
       names. Adding support for a user-defined Org-Id separator would have fixed
       this but with no benefit to Org-Id. So this function removes the colon
       instead.
       "
      (interactive)
      (let* ((gend (org-id-new))
	     (newid (replace-regexp-in-string ":" "_" gend)))
	newid))

    ;; Define babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
	(sql . t)
	(python . t)
	(php . t)
	(sass . t)
	(css . t)
	(js . t)
       ))

    (setq org-capture-templates ())

    (load-user-file ".org-capture-templates.el")

) ;; End orgmode config

(use-package org-mru-clock
  :defer t
  :bind (("C-c o m" . org-mru-clock-in))
  :commands (org-mru-clock-in org-mru-clock-select-recent-task)
  :config
  (setq org-mru-clock-how-many 50
        ;;;;;;;;;;;;;; Also possible: #'ido-completing-read
        org-mru-clock-completing-read #'ivy-completing-read))

;; Additional Orgmode Packages
(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package ox-twbs)
(use-package ox-reveal
    :load-path ("~/src/org-reveal")
    :defer 3
    :after org
    )

(use-package org-journal
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
	    '("j" "Journal" plain
		(function org-journal-find-location)
"
* %^{Title}
  %U
  %i
  %?
"
		:empty-lines 1
		:jump-to-captured t))

(use-package org-roam
      :diminish
      :hook
	(after-init . org-roam-mode)
      :straight (:host github :repo "org-roam/org-roam")
      :custom
	(org-roam-directory "~/org/roam/")
      :bind (:map org-roam-mode-map
		(("C-c n l" . org-roam)
	         ("C-c n f" . org-roam-find-file)
	         ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
		(("C-c n i" . org-roam-insert))))

(defun bt/start-org-roam-server ()
  "Starts the org-roam-server and launches the webpage"
  (interactive)
  (require 'org-roam-protocol)
  (require 'browse-url)
  (server-start)
  (org-roam-server-mode)
  (browse-url "http://127.0.0.1:8080"))

(use-package org-roam-server
  :straight (:host github :repo "org-roam/org-roam-server")
  :config
    (setq org-roam-server-host "127.0.0.1"
	    org-roam-server-port 8080
	    org-roam-server-export-inline-images t
	    org-roam-server-authenticate nil
	    org-roam-server-network-poll t
	    org-roam-server-network-arrows nil
	    org-roam-server-network-label-truncate t
	    org-roam-server-network-label-truncate-length 60
	    org-roam-server-network-label-wrap-length 20))

(use-package deft
  :after org
  :bind
    ("C-c n d" . deft)
  :config
    (setq deft-recursive t
	  deft-default-extension "org"
	  deft-directory "~/org/roam"
	  deft-use-filter-string-for-filename t))

(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.
The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file. A datetree within a subheading
is possible if the heading has a property of DATE_TREE."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date))
         )
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

(eval-after-load "org"
  '(require 'ox-md nil t))

;; set line wrap for text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Writing
;;;;
(use-package flyspell
  :init
    (defun flyspell-check-next-highlighted-word ()
	"Custom function to spell check next highlighted word"
	(interactive)
	(flyspell-goto-next-error)
	(ispell-word))
  :config
    (global-set-key (kbd "C-c ; s m") 'flyspell-mode)
    (global-set-key (kbd "C-c ; s b") 'flyspell-buffer)
    (global-set-key (kbd "C-c ; s w") 'ispell-word)
    (global-set-key (kbd "C-c ; s c") 'flyspell-check-next-highlighted-word)
  :hook
    (text-mode . flyspell-mode))

(use-package writegood-mode
  :config
    (global-set-key (kbd "C-c ; w g m") 'writegood-mode)
    (global-set-key (kbd "C-c ; w g e") 'writegood-reading-ease)
    (global-set-key (kbd "C-c ; w g l") 'writegood-grade-level))

(use-package writeroom-mode
  :config
    (global-set-key (kbd "C-c ; w r m") 'writeroom-mode)
    (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width))

;; Edit Chrome text areas with emacs
(use-package edit-server
  :init
    (setq edit-server-new-frame nil) ;; Open in new buffer of existing frame
  ;; Start the server and use markdown by default
  :config
    (edit-server-start)
    (add-hook 'edit-server-start-hook 'markdown-mode))

;; Tools to get frequency of words
(defun pp/get-word-frequency ()
  "Return an alist with counts for all words in the current buffer."
  (let ((words))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "\\w+" (point-max) t)
	(let ((word (match-string-no-properties 0)))
	  (cl-incf (cdr (or (assoc word words)
			    (first (push (cons word 0) words)))))))
      (reverse (sort words (lambda (a b) (< (cdr a) (cdr b))))))))

(defun pp/show-word-frequency ()
  "Display a word frequency analysis for the current buffer."
  (interactive)
  (let* ((buf (get-buffer-create "*Word Frequency*"))
	 (word-freq (pp/get-word-frequency))
	 (text (concat "Word\t\tCount\n----\t\t-----\n"
		       (mapconcat (lambda (word)
				    (format "%s\t\t%s" (car word) (cdr word)))
				  word-freq "\n"))))
    (with-current-buffer buf
      (erase-buffer)
      (insert text)
      (goto-char 0))
    (pop-to-buffer buf)))
(define-key global-map (kbd "C-c w c") 'pp/show-word-frequency)


;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;;
;; Backups
;;
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

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2    ; and some old ones, too
      auto-save-interval 20)

;; SysAdmin
(use-package restclient)
(use-package graphql-mode)
(use-package graphql)

;; Snippets
(use-package yasnippet)
(use-package yasnippet-snippets)

;; Viewing Logs
(use-package logview)

;; Programming
(use-package flycheck)
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package lsp-mode
 :config
   (setq lsp-prefer-flymake nil)
 :hook
   (php-mode . lsp)
   (python-mode . lsp)
   (sh-mode . lsp)
 :commands
   lsp)

(use-package lsp-ui
    :requires
	lsp-mode flycheck
    :config
	(setq
	    lsp-ui-doc-enable t
	    lsp-ui-doc-use-childframe t
	    lsp-ui-doc-position 'top
	    lsp-ui-doc-include-signature t
	    lsp-ui-sideline-enable nil
	    lsp-ui-flycheck-enable t
	    lsp-ui-flycheck-list-position 'right
	    lsp-ui-flycheck-live-reporting t
	    lsp-ui-peek-enable t
	    lsp-ui-peek-list-width 60
	    lsp-ui-peek-peek-height 25
	    lsp-ui-sideline-enable nil)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
    :commands company-lsp)

;; Shell (Bash, Zsh, sh, etc)
(defun bt/browse-shellcheck-wiki ()
  "When point is on a shellcheck code (ie SC2162), browse the wiki entry for that code."
  (interactive)
  (browse-url
   (concat "https://github.com/koalaman/shellcheck/wiki/" (thing-at-point 'word))))

(add-hook 'sh-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "shellcheck --format=gcc " buffer-file-name))))

;; PHP
(use-package php-mode
  :bind
  (:map php-mode-map
    ("C-c t t" . phpunit-current-test)
    ("C-c t c" . phpunit-current-class)
    ("C-c t p" . phpunit-current-project)))

(use-package composer)
(use-package phpunit)

;; Smartparens
(use-package smartparens)

;; Web Mode
(use-package web-mode
  :bind
  (:map web-mode-map
    ("C-c C-e h" . web-mode-element-sibling-previous)
    ("C-c C-e l" . web-mode-element-sibling-next)
    ("C-c t t" . phpunit-current-test)
    ("C-c t c" . phpunit-current-class)
    ("C-c t p" . phpunit-current-project)))

;; Emmet
(use-package emmet-mode
  :init
    (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode) ;; Enable Emmet's css abbreviation.
    (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any web modes
  )

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\lightning.log\\'" . json-mode))

;; ;;;;
;; ;; Javascript
;; ;;;;

(use-package indium)

(use-package exec-path-from-shell
  :config
    (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package json-mode)

;;;;
;; Python Dev
;;;;

(use-package lsp-python-ms
  :defer 0.3
  :custom (lsp-python-ms-auto-install-server t))

;; ELPY - general python goodness
(use-package elpy
  :init
    (setq python-shell-completion-native-enable nil)
    (elpy-enable)
  :config
    (add-hook 'python-mode-hook
	(lambda ()
	    (local-set-key (kbd "C-c t t") 'elpy-test-pytest-runner))))


;; Manage Pipenv within Emacs
(use-package pipenv
  :hook
    (python-mode . pipenv-mode))

;; EIN - Jupyter Notebook client
(use-package ein
  :config
    (setq ein:output-area-inlined-images t))

;; Productivity
(use-package wakatime-mode
  :diminish
  :config
    (global-wakatime-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-height 20)
 '(org-agenda-files
   (quote
    ("~/Documents/Journal/journal.org" "~/Documents/home-purchase/3900-SW-Pendleton-St-97221_26466082/3900-SW-Pendleton-St-97221_26466082.org" "/mnt/Linux_Data/Documents/passionsplay/blog/blog-planner.org" "~/Documents/passionsplay/passionsplay.org" "~/Documents/benjamin/life.org" "/mnt/Linux_Data/Documents/passionsplay/blog/posts/using-emacs-instead-of-jupyter-notebook.org" "~/Documents/org-solving-with-text/solving-with-text.org")))
 '(package-selected-packages
   (quote
    (exercism exercism-emacs php-mode writeroom-mode writegood-mode elfeed-org ranger ledger-mode python-pytest ace-window ein-subpackages ein-notebook ein ace-jump-mode emmet-mode neotree general which-key helm evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
