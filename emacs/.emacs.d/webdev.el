;; Web Mode
(use-package web-mode
  :hook (web-mode . lsp-deferred)
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

;; Emmet
(use-package emmet
  :straight (:host github :repo "joostkremers/emmet-for-emacs")
  :defer t
  :delight (emmet)
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))
