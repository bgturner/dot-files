;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI Improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Configure time display in modeline
(setq display-time-default-load-average nil)
(setq display-time-day-and-date t)
(setq display-time-format "%F %H:%M %z")
(display-time)

;; Remember window layouts
(winner-mode 1)

(recentf-mode)

;; Make "sentence movements" feel more intuitive
(setq sentence-end-double-space nil)

(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster

;; Splash Screen
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks nil)

;; Navigate directly to specific windows quickly.
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Which Key: Discover what keys are associated with a keymap.
(use-package which-key
  :delight
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-Iosvkem t))

;; use Jetbrains Mono. larger font-size for MacOS
(let ((font-size (if (eq system-type 'darwin)
		     (format "14")
		   (format "11"))))
  (set-frame-font (format "Jetbrains Mono %s" font-size) nil t))

;; Clean up the modeline
(use-package delight
  :config
  (delight '((visual-line-mode)
	     (eldoc-mode nil "eldoc")
	     (yas-minor-mode nil "yasnippet")
	     (hs-minor-mode nil "hideshow")
	     (undo-tree-mode nil "undo-tree")
	     (which-key-mode nil "which-key")
	     (wakatime-mode)
	     (auto-revert-mode nil "autorevert")
             (emacs-lisp-mode "λ" :major)
             (js-mode "JS" :major)
	     )))

;; Completely hide when we need focus
(use-package hide-mode-line
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UX Adjustments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keybinding
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer bt/leader-key-def
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (bt/leader-key-def
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(execute-extended-command :which-key "M-x")

   "t" '(:ignore t :which-key "Toggle")
   "tl" '(toggle-truncate-lines :which-key "Truncate lines")
   "tw" '(whitespace-mode :which-key "Whitespace")

   ;; Placeholder for running tests depending on the language
   "tt" '(:ignore t :which-key "Testing")

   ;; Org-mode
   "o" '(:ignore t :which-key "Org")
   "ob" '(org-switchb :which-key "Switch to Org Buffer")
   "o/" '(org-occur-in-agenda-files :which-key "Occur in Agenda files")
   "oj" '(:ignore t :which-key "Jump")
   "ojc" '(org-clock-goto :which-key "Current Clock")
   "ojh" '(consult-org-agenda :which-key "Headline")
   "oa" '(org-agenda :which-key "Org agenda")
   "ot" '(:ignore t :which-key "Toggle")
   "oth" '(org-toggle-heading :which-key "Heading")
   "oti" '(org-toggle-item :which-key "Item")
   "otl" '(org-toggle-link-display :which-key "Link display")
   "ow" '(:ignore t :which-key "Web Tools")
   "owl" '(org-web-tools-insert-link-for-url :which-key "Insert Link")
   "owr" '(org-web-tools-read-url-as-org :which-key "Read URL")

   ;; "Jump"
   "j" '(:ignore t :which-key "Jump")
   "jb" '(consult-buffer :which-key "Switch Buffer")
   "jm" '(consult-mark :which-key "Evil Marks")
   "jr" '(consult-register :which-key "Evil Registers")

   "e" '(:ignore t :which-key "Errors/Eval")
   "eb" '(eval-buffer :which-key "Eval Buffer")
   ;; Files
   "f" '(:ignore t :which-key "Files")
   "fr"  '(consult-recent-file :which-key "Recent Files")
   "fd"  '(find-dired :which-key "Find Files")

   ;; Buffers
   "b" '(:ignore t :which-key "Buffers")
   "bi"  '(ibuffer :which-key "iBuffer")
   "bb"  '(consult-buffer :which-key "Switch Buffer")

   ;; Others
   "a" '(:ignore t :which-key "Apps")
   "as" '(:ignore t :which-key "Shells")
   "ase"  '(eshell :which-key "Eshell")
   "ass"  '(counsel-switch-to-shell-buffer :which-key "Switch to Shell")
   "ar"  '(bt/restclient-sandbox :which-key "Restclient Sandbox")
   "at" '(:ignore t :which-key "Treemacs")
   "ats" '(treemacs-select-window :which-key "Treemacs Select Window")
   "atd" '(treemacs-delete-other-windows :which-key "Treemacs Delete other windows")
   "att" '(treemacs :which-key "Treemacs")

   ;; Magit
   "g" '(:ignore t :which-key "Magit")
   "gs" '(magit-status :which-key "Status")
   "gd" '(magit-diff-unstaged :which-key "Diff Unstaged")
   "gl" '(:ignore t :which-key "Log")
   "glc" '(magit-log-current :which-key "Current Branch")
   "gla" '(magit-log-all :which-key "All")
   "glf" '(magit-log-buffer-file :which-key "Current File")

   ;; Writing
   "w" '(:ignore t :which-key "Writing")
   "wg" '(:ignore t :which-key "Writegood")
   "wgm" '(writegood-mode :which-key "Toggle Writegood Mode")
   "wgr" '(writegood-reading-ease :which-key "Reading Ease Score")
   "wr" '(:ignore t :which-key "Writeroom")
   "wrm" '(writeroom-mode :which-key "Toggle Writeroom")
   "wc" '(:ignore t :which-key "Word Count")
   "wcm" '(wc-mode :which-key "Toggle Wordcount mode")
   "wcg" '(wc-set-word-goal :which-key "Set Word Goal")
   "wcr" '(wc-reset :which-key "Reset Word Goal")
   "wd" '(:ignore t :which-key "Define")
   "wdp" '(define-word-at-point :which-key "At Point")
   "wdd" '(define-word :which-key "Define Word")
   "wf" '(:ignore :which-key "Focus")
   "wff" '(focus-mode :which-key "Focus Mode")
   "wfc" '(focus-change-thing :which-key "Change")

   "wl" '(winner-undo :which-key "Winner Undo")
   "wk" '(winner-redo :which-key "Winner Redo")
   ))
