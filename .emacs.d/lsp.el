
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
  :init
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point))

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
