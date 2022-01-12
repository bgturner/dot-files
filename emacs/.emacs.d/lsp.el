
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
