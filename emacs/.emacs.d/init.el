;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for loading files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "load a file in current user's configuration directory
   if it exists."
  (let ((fullfile (expand-file-name file user-init-dir)))
    (when (file-exists-p fullfile)
      (load-file fullfile))))

(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packaged management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(let ((system-specific-config (if (eq system-type 'darwin)
				  (format "work-config.el")
				(format "personal-config.el"))))
  (load-user-file system-specific-config))

(load-user-file "ui.el")
(load-user-file "evil.el")
(load-user-file "dired.el")
(load-user-file "general-programming.el")
;; (load-user-file "ivy.el")
(load-user-file "scemop-completion.el")

(load-user-file "org-config.el")
(load-user-file "request-clients.el")
(load-user-file "webdev.el")
(load-user-file "lsp.el")
(load-user-file "php.el")
(load-user-file "js.el")
(load-user-file "rust.el")
(load-user-file "backups.el")


;; Where to find secrets
(setq auth-sources '("~/.authinfo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Separate Customization file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((custom-filename ".custom.el"))
  (when (file-exists-p custom-filename)
    (setq custom-file (expand-file-name custom-filename user-init-dir))
    (load custom-file)))

