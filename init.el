;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
;; (menu-bar-mode -1)

;; Splash Screen
(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 110))

(setq display-time-format "%F %R")
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ivy
  :ensure t
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (bind-key "C-c C-r" 'ivy-resume))

(ivy-mode 1)

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
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
  :ensure t
  :config
    (add-hook 'after-init-hook 'global-company-mode)
  :init
    (global-set-key (kbd "C-x c") 'company-complete-common))

;; (setq counsel-grep-base-command
;;  "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

;; Vim mode
(use-package evil
  :ensure t
  :config
    (evil-mode 1))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :init
  (evil-commentary-mode))

(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode 1))

(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
			    (nrepl-mode . insert)
			    (pylookup-mode . emacs)
			    (comint-mode . normal)
			    (shell-mode . insert)
			    (git-commit-mode . insert)
			    (git-rebase-mode . emacs)
			    (term-mode . emacs)
			    (help-mode . emacs)
			    (image . emacs)
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
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Evil-Magit
(use-package evil-magit
  :ensure t
  :after evil magit)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
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
  "ff" '(counsel-rg :which-key "Ripgrep")
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

;; Increase the number of recent files that are stored
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 50)

;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

(setq projectile-completion-system 'ivy)

;; Ledger for accounting
(use-package ledger-mode
  :ensure t)


;; News and Reading
(use-package pocket-reader
  :ensure t
  :init
    (evil-set-initial-state 'pocket-reader-mode 'emacs)
  )

;; Elfeed for News
;; use an org file to organise feeds
(use-package elfeed-org
  :ensure t
  :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Documents/rss/rss-feeds.org")))

(defun passionsplay/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

(defun passionsplay/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :ensure t
  :init
    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (evil-set-initial-state 'elfeed-show-mode 'emacs)
  :bind
    (:map elfeed-search-mode-map
              ("q" . passionsplay/elfeed-save-db-and-bury)))

;; Writing
(use-package writeroom-mode
  :ensure t
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

(use-package writegood-mode
  :ensure t
  :config
    (global-set-key (kbd "C-c C-g C-c") 'writegood-mode)
    (global-set-key (kbd "C-c C-g C-e") 'writegood-reading-ease)
    (global-set-key (kbd "C-c C-g C-g") 'writegood-grade-level))

;; OrgMode Configs
(use-package org
  :ensure t
  :init
    ; General Settings
    (setq org-hide-emphasis-markers t
	  org-confirm-babel-evaluate nil
	  org-log-into-drawer t
	  org-return-follows-link t
	  org-log-done 'time
	  org-html-validation-link nil
	  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "CANCELED(c@)")))

    ; Agenda Settings
    (setq org-agenda-span 'day
          org-agenda-custom-commands
	  '(("c" . "Custom Agenda Views")
	    ("cp" "Planning, Fourteen day agenda with all unscheduled todos"
	     ((agenda "" ((org-agenda-span 14)
			  (org-agenda-start-on-weekday 0)))
	      (alltodo "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))))

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
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cb" 'org-switchb)
    (global-set-key (kbd "C-c r d") 'org-refile-to-datetree)
    (font-lock-add-keywords            ; A bit silly but my headers are now
     'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
		  (1 (progn (compose-region (match-beginning 1) (match-end 1) "□")
			    nil)))
		 ("^\\*+ \\(NEXT\\) "
		  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⇒")
			    nil)))
		 ("^\\*+ \\(DONE\\) "
		  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
			    nil)))
		 ("^\\*+ \\(CANCELED\\) "
		  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
			    nil)))
		 ))
  )

(use-package org-bullets
  :ensure t
  :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


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

;; Org Exports
(use-package ox-twbs
  :ensure t)

(use-package ox-reveal
  :load-path ("~/src/org-reveal")
  :defer 3
  :after org
  )



(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-return-follows-link t)
(setq org-log-done 'time)
(setq org-html-validation-link nil)

;; set line wrap for text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Define default capture file
(setq org-default-notes-file "~/org/capture.org")

;; Persist Clocking History
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Define capture templates
(setq org-capture-templates
	'(

	("b" "Templates for Ben")
	("bt" "New Task" entry (file+headline "~/Documents/benjamin/life.org" "Tasks")
	    "* TODO %?\n  %i\n  %a")

	("p" "Templates for PassionsPlay")
	("pt" "New Task" entry (file+headline "~/Documents/passionsplay/passionsplay.org" "Tasks")
	    "* TODO %?\n  %i\n  %a")

	("j" "Templates for Journals")
	("je" "New Entry" entry (file+olp+datetree "~/Documents/Journal/journal.org")
	    "**** %?\n     %^{DateTime}T\n     %i\n     %a"
	    :empty-lines 1
	    :clock-in t)

	("f" "Templates for FW")
	("ft" "New Task" entry(file+headline "~/Documents/flywheel/fw.org" "Tasks")
	    "* TODO %?\n %i\n %a")
	("fc" "Calendar" entry(file+headline "~/Documents/flywheel/fw.org" "Calendar")
	    "* %^{DateTime}T %?\n")

	("l" "Local")
	("lt" "New Task" entry(file+headline "~/Documents/local/local.org" "Tasks")
	    "* TODO %?\n %i\n %a")
	("lc" "Calendar" entry(file+headline "~/Documents/local/local.org" "Calendar")
	    "* %^{DateTime}T %?\n")

	))

;; org2blog
(use-package org2blog
  :ensure t)

(setq org2blog/wp-use-sourcecode-shortcode t)
(setq org2blog/wp-sourcecode-langs
    '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
	"erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
	"objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql" "vb"
	"xml" "sh" "emacs-lisp" "lisp" "lua"))

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
  :ensure t
  :config
    (global-set-key (kbd "C-c ; w g m") 'writegood-mode)
    (global-set-key (kbd "C-c ; w g e") 'writegood-reading-ease)
    (global-set-key (kbd "C-c ; w g l") 'writegood-grade-level))

(use-package writeroom-mode
  :ensure t
  :config
    (global-set-key (kbd "C-c ; w r m") 'writeroom-mode)
    (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width))

;; Edit Chrome text areas with emacs
(use-package edit-server
  :ensure t
  :init
    (setq edit-server-new-frame nil) ;; Open in new buffer of existing frame
  ;; Start the server and use markdown by default
  :config
    (edit-server-start)
    (add-hook 'edit-server-start-hook 'markdown-mode))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; SysAdmin
(use-package restclient
  :ensure t)

;; Programming
(use-package flycheck
  :ensure t)

;; Smartparens
(use-package smartparens
  :ensure t)

;; Web Mode
(use-package web-mode
  :ensure t
  :bind
  (:map web-mode-map
    ("C-c C-e h" . web-mode-element-sibling-previous)
    ("C-c C-e l" . web-mode-element-sibling-next)
    ("C-c t t" . phpunit-current-test)
    ("C-c t c" . phpunit-current-class)
    ("C-c t p" . phpunit-current-project)))

;; PHP
(use-package php-mode
  :ensure t)

;; Emmet
(use-package emmet-mode
  :ensure t
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

;;;;
;; Python Dev
;;;;

;; ELPY - general python goodness
(use-package elpy
  :ensure t
  :init
    (elpy-enable)
  :config
    (add-hook 'python-mode-hook
	(lambda ()
	    (local-set-key (kbd "C-c t t") 'elpy-test-pytest-runner))))

;; EIN - Jupyter Notebook client
(use-package ein
  :ensure t)

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
