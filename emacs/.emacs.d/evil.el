;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bt/set-initial-evil-states ()
  "Set the initial Evil states for various modes."
  (cl-loop for (mode . state)
	   in '((inferior-emacs-lisp-mode . emacs)
		(flycheck-error-list-mod . emacs)
		(diff-mode . emacs)
		(nrepl-mode . insert)
		(pylookup-mode . emacs)
		(comint-mode . normal)
		(compilation-mode . emacs)
		(shell-mode . insert)
		(git-commit-mode . insert)
		(git-rebase-mode . emacs)
		(term-mode . emacs)
		(help-mode . emacs)
		(image-mode . emacs)
		(deft-mode . emacs)
		(helm-grep-mode . emacs)
		(grep-mode . emacs)
		(bc-menu-mode . emacs)
		(magit-branch-manager-mode . emacs)
		(rdictcc-buffer-mode . emacs)
		(dired-mode . emacs)
		(elfeed-search . emacs)
		(wdired-mode . normal))
	   do (evil-set-initial-state mode state)))

;; Evil Package
(use-package evil
  :init
  (setq evil-want-C-i-jump nil) ;; Fix tab key in org-mode
  :config
  (evil-mode 1)
  (bt/set-initial-evil-states)
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1))
  (use-package evil-commentary
    :delight
    :init
    (evil-commentary-mode))
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1)))
