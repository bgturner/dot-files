(setq user-emacs-directory "~/.emacs.d.bare")
(setq custom-file "~/.emacs.d.bare/custom.el")
(load custom-file)

(global-set-key [remap list-buffers] 'ibuffer)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 

;; (use-package delight
;;   :config (delight '(
;; 		     (visual-line-mode)
;; 		     (auto-revert-mode nil "autorevert")
;; 		     (emacs-lisp-mode "λ" :major)
;; 		     (typescript-ts-mode "💀" :major)
;; 		     (typescript-mode "💀" :major)
;; 		     )))

(select-frame-set-input-focus (selected-frame))
