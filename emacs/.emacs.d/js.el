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
