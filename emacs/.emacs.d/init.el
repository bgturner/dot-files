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

(use-package esup)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Specific configs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (if (eq system-type 'darwin)
;;     (load-user-file "work-config.el")
;;   (load-user-file "personal-config.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks nil)

;; Theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-Iosvkem t))

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
(use-package hide-mode-line
  :defer t)

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
  (global-set-key (kbd "C-x j m") 'counsel-evil-marks)
  (global-set-key (kbd "C-x j r") 'counsel-evil-registers)
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

;; Where to find secrets
(setq auth-sources '("~/.authinfo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bt/set-initial-evil-states ()
  "Set the initial Evil states for various modes."
  (cl-loop for (mode . state)
	   in '((inferior-emacs-lisp-mode . emacs)
		(diff-mode . emacs)
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

   ;; Placeholder for running tests depending on the language
   "tt" '(:ignore t :which-key "Testing")

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

   ;; "Jump"
   "j" '(:ignore t :which-key "Jump")
   "jb" '(counsel-switch-buffer :which-key "Switch Buffer")
   "jm" '(counsel-evil-marks :which-key "Evil Marks")
   "jr" '(counsel-evil-registers :which-key "Evil Registers")

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

;; (use-package smartparens)

;; Better undo
(use-package undo-fu
  :config
  ;; (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; Company for autocomplete
(use-package company
  :defer t
  :delight
  ;; :config
  ;;   (add-hook 'after-init-hook 'global-company-mode)
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
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
				    :compile "npm install"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix ".spec")
  (projectile-register-project-type 'php '("composer.json")
                                    :project-file "composer.json"
				    :compile "composer install"
				    :test "composer run test"
				    :run "composer run start"
				    :test-suffix "Test.php")
  )

;; Snippets
(use-package yasnippet
  :delight
  ;; :defer t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  ;; :defer t
  )

;; Make align-regex insert spaces instead of tabs
;;     See: https://stackoverflow.com/a/25164056
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  ;; :init
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  )

;; Evil-Magit
(use-package evil-magit
  :defer t
  :after evil magit)

;; Interact with Issues from places like Github
(use-package forge
  :defer t
  :after magit)


(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)))


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
      (insert "# restclient sandbox\n\n#\n"))
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;; Viewing Logs
(use-package logview
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier
;;   Requires: npm install -g prettier
(use-package prettier
  :defer t
  :config
  (add-hook 'typescript-mode-hook 'prettier-mode))

;; Autocomplete paired brackets
(electric-pair-mode 1)

;; View Gherkin files
(use-package feature-mode
  :defer t)

;; ;; Get POSIX shell everywhere
;; (use-package vterm)

;; Code Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; ;; Inherit shell environment
;; (use-package exec-path-from-shell
;;    :config (exec-path-from-shell-initialize))

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
  (typescript-mode . lsp)
  (js-mode . lsp)
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

(use-package dap-mode
  :defer t
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

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
  :hook (php-mode . lsp-deferred)
  :general
  (:states 'normal
	   :prefix "SPC t"
           :keymaps 'php-mode-map
	   "tt" 'phpunit-current-test
	   "tc" 'phpunit-current-class
	   "tp" 'phpunit-current-project)
  :config
  (require 'dap-php)
  (dap-php-setup)
  (use-package phpunit))


;; JS
(use-package typescript-mode
  :defer t
  :delight
  (typescript-mode "TS" :major)
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\lightning[^.]?*.log\\'" . json-mode)))

;; Rust
(use-package rust-mode
  :defer t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(put 'narrow-to-region 'disabled nil)

