;; Ensure we have a .custom.el file and use it to store customizations
(unless (file-exists-p (expand-file-name ".custom.el" user-emacs-directory))
  (write-region "" nil (expand-file-name ".custom.el" user-emacs-directory)))
(setq custom-file (expand-file-name ".custom.el" user-emacs-directory))
(load custom-file)

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

;; Load any system-specific config early
(let ((system-specific-config (if (eq system-type 'darwin)
				  (format "work-config.el")
				(format "personal-config.el"))))
 (load-user-file system-specific-config))

;; Include Melpa so we can install packages with:
;;
;;    M-x package-install
;;
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Allow Straight to manage our packages
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


;; Needed for LSP to work, and maybe other things...
(setq read-process-output-max (* 1024 1024)) ; Set it to 1 MB (or adjust as needed)
(setq gc-cons-threshold (* 2 1024 1024)) ; Set it to 2 MB (or adjust as needed)

(use-package delight
  :config (delight '((visual-line-mode)
		     (auto-revert-mode nil "autorevert")
		     (emacs-lisp-mode "λ" :major))))

(use-package functions
  :straight nil
  :defer t
  :preface
  (require 'subr-x)
  (defun split-pararagraph-into-lines ()
    "Split the current paragraph into lines with one sentence each."
    (interactive)
    (save-excursion
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))
      (let ((auto-fill-p auto-fill-function)
            (end (progn (end-of-line) (backward-sentence) (point))))
        (back-to-indentation)
        (unless (= (point) end)
          (auto-fill-mode -1)
          (while (< (point) end)
            (forward-sentence)
            (delete-horizontal-space)
            (newline-and-indent))
          (deactivate-mark)
          (when auto-fill-p
            (auto-fill-mode t))
          (when (looking-at "^$")
            (backward-delete-char 1))))))
  (defun indirect-narrow-to-defun ()
    (interactive)
    (clone-indirect-buffer (buffer-name) t t)
    (narrow-to-defun))
  (defun indirect-narrow-to-region ()
    (interactive)
    (let ((beg (mark))
          (end (point)))
      (clone-indirect-buffer (buffer-name) t t)
      (narrow-to-region beg end)))
  (defun edit-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))
  (defun bt/occur-describe-it ()
    "Show lines that match describe and it function signatures
like the ones used by Jest."
    (interactive)
    (occur "\\(\\s ?+describe(\\|\\s ?+it(\\)")
    (other-window 1)
    (follow-mode t))


  (provide 'functions))

(use-package defaults
  :straight nil
  :defer t
  :preface
  (use-package emacs
    :init
    (menu-bar-mode -1)
    (toggle-scroll-bar -1)
    (tool-bar-mode -1)
    (tooltip-mode -1)
    (scroll-bar-mode -1)
    (global-hl-line-mode +1)
    (blink-cursor-mode -1)
    (line-number-mode +1)
    (column-number-mode t)
    (size-indication-mode t)
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq-default tab-width 4)
    (setq-default indent-tabs-mode nil)
    (setq inhibit-startup-screen t)
    )

  (show-paren-mode t)
  ; Allow minibuffers within minibuffers
  enable-recursive-minibuffers t

  ;; Configure time display in modeline
  (setq display-time-default-load-average nil)
  (setq display-time-day-and-date t)
  (setq display-time-format "%F %H:%M %z")
  (display-time)

  ;; Remember window layouts
  (winner-mode 1)

  ;; Remember recently viewed files
  (setq recentf-max-saved-items 300
	recentf-max-menu-items 300)
  (recentf-mode)

  ; Make "sentence movements" feel more intuitive
  (setq sentence-end-double-space nil)

  ; Make *scratch* buffer blank
  (setq initial-scratch-message "")

  ; Disable bell sound
  (setq ring-bell-function 'ignore)

  ; y-or-n-p makes answering questions faster
  (fset 'yes-or-no-p 'y-or-n-p)

  (provide 'defaults))


(use-package use-package-hydra)

(use-package ui
  :straight nil
  :defer t
  :preface
  ;; Theme
  (use-package doom-themes
    :config
    ;; (load-theme 'doom-one t)
    (load-theme 'doom-dracula t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    )

  ;; Fonts - use Jetbrains Mono. larger font-size for MacOS
  (let ((font-size (if (eq system-type 'darwin)
		       (format "14")
		     (format "11"))))
    (set-frame-font (format "JetbrainsMono Nerd Font %s" font-size) nil t))

  ;; Completely hide when we need focus
  (use-package hide-mode-line
    :defer t)

  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
				 "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
				 "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
				 "</" "<*")
                             (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
				 "|]" "|}" "|=")
                             (+ "|"))))
             (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  . ,(rx (or "+>" (+ "+"))))
             (?\[ . ,(rx (or "[<" "[|")))
             (?\{ . ,(rx "{|"))
             (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                             (+ "#"))))
             (?\; . ,(rx (+ ";")))
             (?_  . ,(rx (or "_|_" "__")))
             (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  . ,(rx "$>"))
             (?^  . ,(rx "^="))
             (?\] . ,(rx "]#"))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring]))))

  (global-auto-composition-mode)

  (provide 'ui))

(use-package ux
  :straight nil
  :preface

  ;; Evil Package
  (use-package evil
    :init
    (setq evil-want-C-i-jump nil) ;; Fix tab key in org-mode
    :config
    (evil-set-undo-system 'undo-redo)
    (evil-mode 1)
    (evil-define-key 'normal diff-hl-mode-map "]v" 'diff-hl-next-hunk)
    (evil-define-key 'normal diff-hl-mode-map "[v" 'diff-hl-previous-hunk)
    (use-package evil-surround
      :init
      (global-evil-surround-mode 1))
    (use-package evil-commentary
	:delight
	:init
	(evil-commentary-mode))
    (use-package evil-matchit
      :init
      (global-evil-matchit-mode 1))

    (defun bt/set-initial-evil-states ()
      "Set the initial Evil states for various modes."
      (cl-loop for (mode . state)
	       in '((inferior-emacs-lisp-mode . emacs)
            (vc-git-log-edit-mode . emacs)
		    (flycheck-error-list-mod . emacs)
		    (diff-mode . emacs)
		    (nrepl-mode . insert)
		    (pylookup-mode . emacs)
		    (comint-mode . normal)
		    (compilation-mode . emacs)
		    (shell-mode . insert)
		    (vterm-mode . emacs)
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
            (org-roam-mode . emacs)
		    (elfeed-search . emacs)
		    (wdired-mode . normal))
	       do (evil-set-initial-state mode state))))
  (bt/set-initial-evil-states)

  (use-package vundo
    :commands (vundo)
    :straight (vundo :type git :host github :repo "casouri/vundo")
    :config
    (setq vundo-compact-display t))
  (with-eval-after-load 'evil (evil-define-key 'normal 'global (kbd "C-c u") 'vundo))

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

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package general
    :config
    (general-evil-setup t)
    
    (general-create-definer bt/leader-key-def
			    :keymaps '(normal visual insert emacs)
			    :prefix "SPC"
			    :global-prefix "M-SPC")
    
    (bt/leader-key-def
     "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
     "SPC" '(execute-extended-command :which-key "M-x")
     
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
     "ojh" '(consult-org-agenda :which-key "Headline")
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
     "jl" '(consult-line :which-key "Line")
     "ja" '(avy-goto-char :which-key "Avy Goto Char")
     "jb" '(consult-buffer :which-key "Switch Buffer")
     "jm" '(consult-mark :which-key "Evil Marks")
     "jr" '(consult-register :which-key "Evil Registers")
     
     "e" '(:ignore t :which-key "Errors/Eval")
     "eb" '(eval-buffer :which-key "Eval Buffer")
     "ep" '(flycheck-previous-error :which-key "Prev")
     "en" '(flycheck-next-error :which-key "Next")
     "el" '(flycheck-list-errors :which-key "List")
     
     ;; Compilation
     "c" '(:ignore t :which-key "Compilation")
     "cc" '(recompile :which-key "Re-Compile")
     
     ;; Files
     "f" '(:ignore t :which-key "Files")
     "fr"  '(consult-recent-file :which-key "Recent Files")
     "fd"  '(find-dired :which-key "Find Files")
     
     ;; Buffers
     "b" '(:ignore t :which-key "Buffers")
     "bi"  '(ibuffer :which-key "iBuffer")
     "bb"  '(consult-buffer :which-key "Switch Buffer")
     
     ;; Others
     "a" '(:ignore t :which-key "Apps")
     "as" '(:ignore t :which-key "Shells")
     "ase"  '(eshell :which-key "Eshell")
     "ass"  '(counsel-switch-to-shell-buffer :which-key "Switch to Shell")
     "ar"  '(bt/restclient-sandbox :which-key "Restclient Sandbox")
     "at" '(:ignore t :which-key "Treemacs")
     "ats" '(treemacs-select-window :which-key "Treemacs Select Window")
     "atd" '(treemacs-delete-other-windows :which-key "Treemacs Delete other windows")
     "att" '(treemacs :which-key "Treemacs")
     
     ;; Magit
     "g" '(magit-file-dispatch :which-key "Magit")
     
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
     
     "wl" '(winner-undo :which-key "Winner Undo")
     "wk" '(winner-redo :which-key "Winner Redo")
     ))
  (provide 'ux))

(use-package dired
  :straight nil
  :defer t
  :config
  (when (eq system-type 'darwin)
    ;; installed with `brew install coreutils`
    ;; use proper GNU ls
    (setq insert-directory-program "/opt/homebrew/bin/gls"))

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
		("/" . dired-narrow))))

(use-package completions
  :straight nil
  :preface
  (use-package vertico
    :init
    (setq enable-recursive-minibuffers t)
    (vertico-mode))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :init
    (savehist-mode))

  ;; Better sorting of candidates
  (use-package prescient)

  ;; Match search candidates in any order
  (use-package orderless
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

  ;; Enable richer annotations using the Marginalia package
  (use-package marginalia
    :bind ("M-A" . marginalia-cycle)
    :init
    (marginalia-mode))

  ;; Example configuration for Consult
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c k" . consult-kmacro)

           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame

           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("<help> a" . consult-apropos)            ;; orig. apropos-command

           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)

           ;; M-s bindings (search-map)
           ("M-s d" . consult-find)
           ("M-s D" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)

           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)
    
    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)
    
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    
    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config
    
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key (kbd "M-.")
     :preview-key '(:debounce 0.4 any))
    
    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")
    )
  
    
  ;; Embark: Perform actions on lists
  (use-package embark
    :bind
    (("C-'" . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

    (use-package embark-consult
      :hook
      (embark-collect-mode . consult-preview-at-point-mode)))

  (use-package company
    :delight
    :hook (after-init . global-company-mode)
    :bind (("C-M-i" . company-complete))
    :config
    (use-package company-lsp))

  (provide 'completions))

(use-package general-programming
  :straight nil
  :preface

  (use-package imenu-list)

  (use-package tree-sitter
    :delight
    :ensure t
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

  (use-package tree-sitter-langs
    :ensure t
    :after tree-sitter)

  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))
  
  (use-package hideshow
    :delight hs-minor-mode
    :hook (prog-mode . hs-minor-mode))

  (use-package eldoc
    :delight eldoc-mode)

  (use-package compile
    :init
    (progn
      (setq compilation-scroll-output t)))

  (use-package wgrep)

  (use-package noccur)

  ;; Projectile
  (use-package projectile
    :delight
    :init
    ;; Make Projectile usable even outside of project roots
    (setq projectile-require-project-root nil
          projectile-switch-project-action #'projectile-dired
          projectile-completion-system 'default
          projectile-per-project-compilation-buffer t)
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :config
    (projectile-mode 1)
    (projectile-register-project-type 'npm '("package.json")
                                      :project-file "package.json"
				      :compile "npm install"
				      :test "npx jest"
				      :run "npm start"
				      :test-suffix ".spec"))

  (use-package yasnippet
    :delight yas-minor-mode
    :config
    (yas-global-mode 1)
    (use-package yasnippet-snippets))

  ;; Make align-regex insert spaces instead of tabs
  ;;     See: https://stackoverflow.com/a/25164056
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it))

  ;; Magit
  (use-package magit
    :defer t
    :init (setq magit-refresh-status-buffer nil)
    ;; :bind (("C-x g" . magit-status))
    (use-package evil-magit
      :defer t
      :after evil magit))

  ;; View/Open a new PR on Github from within a Magit related buffer
  ;; See:
  ;;     https://prathamesh.tech/2019/06/21/creating-pull-requests-from-emacs/
  (defun bt/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch))))
  (eval-after-load 'magit
    '(define-key magit-mode-map "v"
                 #'bt/visit-pull-request-url))

  (defun bt/find-pull-request (commit-hash)
    (interactive "sCommit hash: ")
    (browse-url (format
                 "https://github.com/search?q=%s&type=pullrequests"
                 commit-hash)))

  (use-package browse-at-remote
    :defer t
    :bind (("C-c g g" . browse-at-remote)))

  ;; Prettier
  ;;   Requires: npm install -g prettier
  (use-package prettier
    :defer t
    :delight
    :hook (after-init . global-prettier-mode))

  ;; Inherit shell environment
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize))
  
  (use-package deadgrep
    :init
    (evil-set-initial-state 'deadgrep-mode 'emacs)
    :general
    (bt/leader-key-def "D" '(deadgrep :which-key "Deadgrep")))
  
  (use-package flycheck
    :delight
    :defer t)
  
  (use-package editorconfig
    :delight
    :config
    (editorconfig-mode 1))
  
  ;; Show matching parens
  (use-package smartparens
    :delight
    :init
    (setq show-paren-delay 0)
    :config
    (progn
      (require 'smartparens-config)
      (smartparens-global-mode 1)
      (show-paren-mode t)))

  (use-package diff-hl
    :config
    (global-diff-hl-mode))

  (use-package wakatime-mode
    :delight wakatime-mode
    :config
    (global-wakatime-mode))
  
  (use-package ansi-color
    :config
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
	(ansi-color-apply-on-region compilation-filter-start (point-max))))
    :hook (compilation-filter . my-colorize-compilation-buffer))


  (use-package treemacs
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (progn

      (custom-set-faces '(treemacs-root-face ((t (:inherit font-lock-string-face :weight bold :height 1.0)))))
      (treemacs-resize-icons 14)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode 'always)

      (pcase (cons (not (null (executable-find "git")))
                   (not (null treemacs-python-executable)))
	(`(t . t)
	 (treemacs-git-mode 'deferred))
	(`(t . _)
	 (treemacs-git-mode 'simple)))

      (treemacs-hide-gitignored-files-mode nil))
    :bind
    (:map global-map
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t d"   . treemacs-select-directory)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-evil
    :after (treemacs evil))

  (use-package treemacs-projectile
    :after (treemacs projectile))

  (use-package treemacs-magit
    :after (treemacs magit))

  (provide 'general-programming))  

(use-package codemetrics
  :straight (codemetrics :type git :host github :repo "jcs-elpa/codemetrics"))

;; Save in ~/.authinfo.gpg
;;     machine api.openai.com password sk-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
(use-package gptel
  :bind (("C-c a" . gptel-send)
         ("C-c c" . gptel))
  :init (setq gptel-default-mode 'org-mode)
  :config
  (setq gptel-model "mistral:latest"
        gptel-backend (gptel-make-ollama "Ollama"
                                         :host "localhost:11434"
                                         :stream t
                                         :models '("mistral:latest"))))

;; Trying out some ideas/bindings from:
;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; https://github.com/rksm/copilot-emacsd/blob/master/init.el
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :delight
  :ensure t
  :bind
  (:map copilot-mode-map
        ("M-C-'" . copilot-complete)
        ("M-C-;" . copilot-accept-completion)
        ("M-C-<down>" . copilot-next-completion)
        ("M-C-<up>" . copilot-previous-completion)
        ("M-C-<left>" . copilot-panel-complete)
        ("M-C-<right>" . copilot-accept-completion-by-word)))

(use-package devops
  :straight nil
  :preface
  (use-package kubernetes
    :ensure t
    :commands (kubernetes-overview)
    :config
    (setq kubernetes-poll-frequency 3600
          kubernetes-redraw-frequency 3600))

  (use-package dockerfile-mode)

  (use-package docker)

  (use-package yaml-mode)

  (use-package yaml-pro
    :after yaml-mode
    :hook (yaml-mode . yaml-pro-mode)
    :general (:keymaps 'yaml-pro-mode-map
	      :states 'normal
	      "zc" #'yaml-pro-fold-at-point
	      "zo" #'yaml-pro-unfold-at-point))

  (provide 'devops))

(use-package lsp
  :straight nil
  :preface
  (use-package lsp-mode
    :config
    (lsp-enable-which-key-integration t)
    (defun bt/clear-lsp-workspace-folders ()
      "Clears the registered workspaces for the current lsp-session.

I was getting 'Creating Pip: Too many files open' errors. It turns out
I had too many projects registered along with too many projects open
at one time.

Running this occasionally zeros out repos that I no longer need so
that I can re-add any projects that I'm actively working on. See:
- https://emacs.stackexchange.com/q/52967/20510
- https://www.reddit.com/r/emacs/comments/ty5i2l/emacs_28_29_watching_eln_files_and_causing_too/
"
      (interactive)
      (dolist (workspace (lsp-session-folders (lsp-session)))
	(lsp-workspace-folders-remove workspace)))
    :hook
    (php-mode . lsp)
    (typescript-mode . lsp)
    (typescript-ts-mode . lsp)
    (web-mode . lsp)
    (js-mode . lsp)
    ;; (python-mode . lsp)
    ;; (sh-mode . lsp)
    :init
    (setq lsp-keymap-prefix "C-c l")
    (setq company-minimum-prefix-length 1
	  company-idle-delay 2.0)
    :commands
    (lsp lsp-deferred))
  
  (use-package lsp-ui
    :bind (("C-h ." . lsp-ui-doc-focus-frame)
	   ("C-c z" . lsp-ui-doc-unfocus-frame))
    :init
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-ui-doc-delay 2.0)
    (setq lsp-ui-doc-show-with-mouse t)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-position 'at-point))
  
  (use-package dap-mode
    :commands dap-debug
    :config
    (require 'dap-node) ;; Set up Node debugging
    (dap-node-setup)    ;; Automatically installs Node debug adapter if needed
    (require 'dap-hydra)
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup)
    ;; Bind `C-c l d` to `dap-hydra` for easy access
    (general-define-key
     :keymaps 'lsp-mode-map
     :prefix lsp-keymap-prefix
     "d" '(dap-hydra t :wk "debugger")))
  
  (defun bt/lsp-file-notify-rm-all-watches ()
    "Remove all existing file notification watches from Emacs."
    (interactive)
    (maphash
     (lambda (key _value)
       (file-notify-rm-watch key))
     file-notify-descriptors))
  
  (provide 'lsp))

(use-package languages
  :straight nil
  :preface
  (use-package graphviz-dot-mode
    :defer t)
  
  ;; Install marked with:
  ;;     npm install -g marked
  (use-package markdown-mode
    :defer t
    :init (setq markdown-command "marked")
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)))

  (use-package graphql-mode
    :defer t)
  
  (use-package graphql
    :defer t)


  (use-package typescript-mode
    :defer t
    :delight (typescript-mode "TS" :major)
    :mode "\\.tsx?\\'"
    :init (setq typescript-indent-level 2))

  (use-package json-mode
    :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\lightning[^.]?*.log\\'" . json-mode)))

  ;; shell
  (use-package flymake-shellcheck
    :commands flymake-shellcheck-load
    :init
    (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

  (use-package vterm)

  ;; Web Mode
  (use-package web-mode
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

  ;; Python
  (use-package poetry
    :defer t)

  ;; PHP
  (use-package php-mode
    :defer t
    :config
    (use-package phpunit))

  
  ;; Emmet
  (use-package emmet
    :defer t
    :straight (:host github
		     :repo "joostkremers/emmet-for-emacs"
		     :files ("*.el" "conf"))
    :delight emmet-mode
    :init
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'typescript-mode-hook 'emmet-mode))
  
  ;; Rust
  (use-package rust-mode
    :defer t)

  (provide 'languages))

(use-package apps
  :straight nil
  :preface
  
  ;; Ledger for accounting
  (use-package ledger-mode
    :defer t)
  
  ;; News and Reading
  (use-package pocket-reader
    :defer t
    :init (evil-set-initial-state 'pocket-reader-mode 'emacs))
  
  (provide 'apps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :init
  ;; General Settings
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
        org-global-properties '(("Effort_ALL" . "0 0:15 0:30 1:00 2:00 4:00 8:00"))
	org-capture-templates '(("i" "Inbox Task" entry (file+headline "~/org/inbox.org" "Tasks")
				 "* TODO %?\n  %i\n  %a")))
  
  ;; Agenda Settings
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$")
	org-agenda-span 'day
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
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (dot . t)
				 (shell . t)
				 (sql . t)
				 (python . t)
				 (css . t)
				 (js . t)))
  
  (load-user-file ".org-capture-templates.el")

  (bt/set-org-agenda-files)
  
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
          (widen)))))
  
  (use-package org-projectile
    :config
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath "todos.org")
    (global-set-key (kbd "C-c o n p") 'org-projectile-project-todo-completing-read)
    (setq org-projectile-capture-template "* TODO %?\n  %a"))
  
  (use-package org-mru-clock
    :bind* (("C-c C-x i" . org-mru-clock-in)
            ("C-c C-x C-j" . org-mru-clock-select-recent-task))
    :config
    (setq org-mru-clock-how-many 100)
    (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))
  
  (use-package org-habit-plus
    :defer t
    :straight (:host github :repo "myshevchuk/org-habit-plus")
    :init
    (setq org-habit-graph-column 60
	  org-habit-preceding-days 10
	  org-habit-show-habits-only-for-today nil
	  org-habit-show-done-always-green t)
    :config
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit))
  
  ;; (use-package org-ql
  ;;   :straight (:host github :repo "alphapapa/org-ql"))
  
  (require 'org-tempo) ; needed to make <s<tag> expand to src blocks in macos
  
  (use-package org-journal
    :defer t
    :bind
    ("C-c n j" . org-journal-new-entry)
    :custom
    (org-journal-dir "~/org/journal/")
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
    (org-roam-directory (file-truename "~/OrgRoam"))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture))
    :config
    (org-roam-db-autosync-mode)
    :general
    (general-nmap "SPC o r" 'hydra-org-roam/body)
    :hydra
    (hydra-org-roam ()
		    "
  _n_ : Next Daily
  _p_ : Prev Daily
  _g_ : Goto Daily
  _c_ : Capture Daily
"
		    ("n" org-roam-dailies-goto-next-note)
		    ("p" org-roam-dailies-goto-previous-note)
		    ("c" org-roam-dailies-capture-date)
		    ("g" org-roam-dailies-goto-date)
		    ))
  
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
  
  (use-package ob-typescript)

  (use-package ob-async)
  
  (eval-after-load "org"
    '(require 'ox-md nil t))
  
  ;; Colorize org-babel output
  ;; https://emacs.stackexchange.com/a/63562
  ;;
  ;; So something like this produces nice, escaped output:
  ;;
  ;;     #+begin_src shell :session my-session-name :results raw :wrap shell_output
  ;;       mkdir -p ~/src/installing-minikube
  ;;       cd ~/src/installing-minikube
  ;;     #+end_src
  ;;
  ;;     #+RESULTS:
  ;;     #+begin_shell_output
  ;;     mkdir -p ~/src/my-sandbox
  ;;     ★  ~ % cd ~/src/my-sandbox
  ;;     #+end_shell_output
  ;;
  (defun bt/babel-ansi ()
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
	(goto-char beg)
	(when (looking-at org-babel-result-regexp)
	  (let ((end (org-babel-result-end))
		(ansi-color-context-region nil))
	    (ansi-color-apply-on-region beg end))))))
  (add-hook 'org-babel-after-execute-hook 'bt/babel-ansi)

  ) ;; End orgmode config


(use-package rest
  :straight nil
  :preface
  (use-package restclient
    :defer t)
  
  (defun bt/restclient-sandbox ()
    "Create a new restclient sandbox to explore urls."
    (interactive)
    (let ((buffer (generate-new-buffer "restclient-sandbox.http")))
      (with-current-buffer buffer
	(restclient-mode)
	(insert "# restclient sandbox\n\n#\n"))
      (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

  (provide 'rest))

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

(use-package flyspell
  :defer t
  :delight
  :init
    (defun flyspell-check-next-highlighted-word ()
	"Custom function to spell check next highlighted word"
	(interactive)
	(flyspell-goto-next-error)
	(ispell-word))
  :general
  (general-nmap "SPC w s" 'hydra-flyspell/body)
  :hydra
  (hydra-flyspell ()
		  "
_m_ Enable Flyspell Mode
_b_ Check spelling in buffer
_c_ Check next highlighted word
_n_ Goto Next error
_p_ Goto Prev error
_a_ Auto-correct
"
		  ("m" flyspell-mode)
		  ("b" flyspell-buffer)
		  ("c" flyspell-check-next-highlighted-word)
		  ("n" flyspell-goto-next-error)
		  ("n" evil-next-flyspell-error)
		  ("p" evil-prev-flyspell-error)
		  ("a" flyspell-auto-correct-word)
		  )
    
  :config
    (when (executable-find "hunspell")
	(setq-default ispell-program-name "hunspell")
	(setq ispell-really-hunspell t))
  :hook
    (text-mode . flyspell-mode))

;; Edit Chrome textareas in Emacs
(use-package edit-server
  :init
    (setq edit-server-new-frame nil) ;; Open in new buffer of existing frame
  ;; Start the server and use markdown by default
  :config
    (edit-server-start)
    (add-hook 'edit-server-start-hook 'markdown-mode))
