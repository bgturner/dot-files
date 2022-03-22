
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
