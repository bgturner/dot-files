(setq user-emacs-directory "~/.emacs.d.bare")

(setq custom-file "~/.emacs.d.bare/custom.el")
(load custom-file)

(setq use-package-always-ensure t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(global-set-key [remap list-buffers] 'ibuffer)

(use-package emacs
  :init
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  (setq ring-bell-function 'ignore)
  (setq enable-recursive-minibuffers t)

  ;; Ask for GnuPG password in minibuffer instead of popup
  (setq epa-pinentry-mode 'loopback)

  :config
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (line-number-mode +1)
  (column-number-mode t)
  (size-indication-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Remember window layouts
  (winner-mode 1)
  (load-theme 'misterioso))

(use-package evil
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (evil-define-key 'normal 'global "]q" 'next-error)
  (evil-define-key 'normal 'global "[q" 'previous-error)
  (evil-define-key 'normal diff-hl-mode-map "]v" 'diff-hl-next-hunk)
  (evil-define-key 'normal diff-hl-mode-map "[v" 'diff-hl-previous-hunk))

(use-package vundo
  :init
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-compact-display t)
  :commands (vundo)
  :bind (("C-c u" . vundo)))

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package magit
  :defer t
  :bind (("C-c g" . magit-file-dispatch))
  :init (setq magit-refresh-status-buffer nil))

(use-package diff-hl
  :after evil
  :config
  (global-diff-hl-mode))

(use-package minions
  :config
  (minions-mode t))

(use-package corfu
  :config
  (global-corfu-mode t))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vertico
  :init
  (setq enable-recursive-minibuffers t)
  :config
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic partial-completion emacs22)))

(use-package prescient)

(use-package marginalia
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

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
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package hideshow
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

(use-package flycheck
  :defer t)

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))
