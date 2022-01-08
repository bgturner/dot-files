;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config for non-work packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ledger for accounting
(use-package ledger-mode
  :defer t)

;; News and Reading
(use-package pocket-reader
  :defer t
  :init
    (evil-set-initial-state 'pocket-reader-mode 'emacs))

;; Elfeed News
(use-package elfeed-org
  :defer t
  :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Documents/personal/rss/rss-feeds.org")))

(defun pp/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

(defun pp/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer."
  (interactive)
  (elfeed-db-save)
  (quit-window))

(use-package elfeed
  :defer t
  :init
    (evil-set-initial-state 'elfeed-search-mode 'emacs)
    (evil-set-initial-state 'elfeed-show-mode 'emacs)
    (setq elfeed-db-directory "~/Sync/personal/rss/.elfeed")
  :bind
    (:map elfeed-search-mode-map
              ("q" . pp/elfeed-save-db-and-bury)))

(use-package elfeed-score
  :defer t
  :ensure t
  :init
    (setq elfeed-score-score-file "~/Sync/personal/rss/elfeed.score")
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

(defun bt/set-org-agenda-files ()
  "Generates the list of Org agenda files."
  (interactive)
  (setq org-agenda-files
	(apply 'append (mapcar (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '(
				 "~/Sync/personal/"
				 "~/Private/"
				 "~/Sync/learning/"
				 "~/Sync/oss/"
				 "~/Sync/fw/"
				 ))))
  ;; Ensure that the general org inbox is part of our agenda
  (push '"~/Sync/org/inbox.org" org-agenda-files))
