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
