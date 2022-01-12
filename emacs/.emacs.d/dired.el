
;; Dired
(when (eq system-type 'darwin)
  ;; installed with `brew install coreutils`
  ;; use proper GNU ls
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package dired+
    :init
    (progn
	;; Details toggling is bound to "(" in `dired-mode' by default
	(setq diredp-hide-details-initially-flag t)
	(setq dired-listing-switches "-aghBo --group-directories-first")))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "h" 'dired-hide-dotfiles-mode))

;;narrow dired to match filter
(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))
