
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
  (setq projectile-per-project-compilation-buffer t)
  (setq projectile-completion-system 'ivy)
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
				    :compile "npm install"
				    :test "npx jest --color"
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

;; Magit
(use-package magit
  :defer t
  :init (setq magit-refresh-status-buffer nil)
  :bind (("C-x g" . magit-status)))

(use-package diff-hl
  :config
    (global-diff-hl-mode))

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


;; Prettier
;;   Requires: npm install -g prettier
(use-package prettier
  :defer t
  :delight
  :hook ((after-init . global-prettier-mode)))

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
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Better undo
(use-package undo-fu
  :config
  ;; (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-tree                    ; Enable undo-tree, sane undo/redo behavior
  :init (global-undo-tree-mode))

(use-package graphviz-dot-mode)
