;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config for non-work packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bt/set-org-agenda-files ()
  "Generates the list of Org agenda files."
  (interactive)
  (setq org-agenda-files
	(apply 'append (mapcar (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '(
				 "~/org/personal/"
				 "~/org/learning/"
				 "~/org/oss/"
				 "~/org/passionsplay/"
				 "~/org/finances/"
				 "~/org/cpto/"
				 ))))
  ;; Ensure that the general org inbox is part of our agenda
  (push '"~/org/inbox.org" org-agenda-files))
