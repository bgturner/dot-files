;; JS
(use-package typescript-mode
  :defer t
  :delight
  (typescript-mode "TS" :major)
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(use-package json-mode
  :config
  (add-to-list 'auto-mode-alist '("\\lightning[^.]?*.log\\'" . json-mode)))

(use-package jest
  :general
  (:states 'normal
	   :prefix "SPC t"
           :keymaps '(js-mode-map typescript-mode-map)
	   "tt" 'jest-function
	   "tf" 'jest-file
	   "tr" 'jest-repeat)
  )

