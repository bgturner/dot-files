;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config for work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Local specific functions
(defconst bt/local-compilation-build-cmd "yarn nps build.dev")
(defconst bt/local-compilation-default-cmd "yarn nps")
(defconst bt/local-compilation-debug-cmd "yarn nps debug")

(defun bt/run-compile (cmd &optional buffer-name)
  "Run a compilation command and rename the buffer if specified.
    CMD is the command to run, BUFFER-NAME is the new name for the compilation buffer."
  (compile cmd)
  (switch-to-buffer "*compilation*")
  (when buffer-name
    (rename-buffer buffer-name)))


;; Local specific functions
(defun bt/local-compilation ()
  "Spawns two compilation buffers to launch Local."
  (interactive)
  (let ((nps-cmd (if current-prefix-arg bt/local-compilation-debug-cmd bt/local-compilation-default-cmd)))
    (project-dired)  ; Open projectile dired
    ;; Run build.dev compilation
    (bt/run-compile bt/local-compilation-build-cmd "*compilation-local-nps-build.dev*")

	(project-dired)

    ;; Run nps or nps debug compilation based on prefix argument
    (bt/run-compile nps-cmd (if current-prefix-arg "*compilation-local-nps-debug*" "*compilation-local-nps*"))

    ;; Switch back to build.dev compilation buffer
    (switch-to-buffer "*compilation-local-nps-build.dev*")))

(defun bt/kill-local-compilation ()
  "Kills the two compilation buffers related to Local."
  (interactive)
  (kill-buffer "*compilation-local-nps-build.dev*")
  (kill-buffer "*compilation-local-nps*")
  (kill-buffer "*compilation-local-nps-debug*"))

(defun bt/open-local-config-folder ()
  "Opens a Dired buffer of the Local config folder"
  (interactive)
  (dired "~/Library/Application Support/Local/" nil))

(defun bt/open-local-log ()
  "Opens the Local Log"
  (interactive)
  (find-file "~/Library/Logs/local-lightning-verbose.log"))
