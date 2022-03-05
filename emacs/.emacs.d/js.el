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

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun bt/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'bt/use-eslint-from-node-modules)
