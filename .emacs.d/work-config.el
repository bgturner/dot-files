;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config for work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Local specific functions
(defun bt/local-compilation ()
  "Spawns two compilation buffers to launch Local."
  (interactive)
  (projectile-dired)
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
