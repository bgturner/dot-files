;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config for work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bt/set-org-agenda-files ()
  "Generates the list of Org agenda files."
  (interactive)
  (setq org-agenda-files
	(apply 'append (mapcar (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '(
				 "~/WPESync/org/"
				 ))))
  ;; Ensure that the general org inbox is part of our agenda
  (push '"~/WPESync/inbox.org" org-agenda-files))

(menu-bar-mode 1)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Set coding font with ligature support
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 '(default ((t (:height 140 :family "JetBrains Mono")))))

;; Local specific functions
(defun bt/local-compilation ()
  "Spawns two compilation buffers to launch Local."
  (interactive)
  ;; Ensure that the general org inbox is part of our agenda
  ;; (projectile-switch-project-by-name "flywheel-local")
  ;; (bt/kill-local-compilation)
  (projectile-dired)
  ;; (make-frame)
  (compile "yarn nps build.dev")
  (switch-to-buffer "*compilation*")
  (rename-buffer "*compilation-local-nps-build.dev*")
  (switch-to-buffer "flywheel-local")
  (compile "yarn nps")
  (switch-to-buffer "*compilation*")
  (rename-buffer "*compilation-local-nps*")
  (switch-to-buffer "*compilation-local-nps-build.dev*"))

(defun bt/kill-local-compilation ()
  "Kills the two compilation buffers related to Local."
  (interactive)
  (kill-buffer "*compilation-local-nps-build.dev*")
  (kill-buffer "*compilation-local-nps*"))

(defun bt/open-local-config-folder ()
  "Opens a Dired buffer of the Local config folder"
  (interactive)
  (dired "~/Library/Application Support/Local/" nil))

(defun bt/open-local-log ()
  "Opens the Local Log"
  (interactive)
  (find-file "~/Library/Logs/local-lightning-verbose.log"))


;; (global-set-key (kbd "C-c C-l") 'counsel-find-file)
