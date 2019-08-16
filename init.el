;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Splash Screen
(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 110))

(setq display-time-format "%F %R")
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ivy
  :ensure t
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (bind-key "C-c C-r" 'ivy-resume))

(ivy-mode 1)

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c a") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :custom
    (ivy-height 20 "number of result lines to display")
  )

;; (setq counsel-grep-base-command
;;  "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :init
  (evil-commentary-mode))

(use-package evil-matchit
  :ensure t
  :init
  (global-evil-matchit-mode 1))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Evil-Magit
(use-package evil-magit
  :ensure t
  :after evil magit)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "SPC" '(counsel-M-x :which-key "M-x")

  ;; Projectile
  "pp"   '(projectile-switch-project :which-key "Switch project")
  "pd"   '(projectile-dired-other-window :which-key "Open dired in new window")
  "pff"  '(projectile-find-file :which-key "Find files in project")
  "pfd"  '(projectile-find-dir-other-window :which-key "Open project dired in other window")

  ;; Org-mode
  "oj" '(counsel-org-agenda-headlines :which-key "Org Jump to Headline")
  "oa" '(org-agenda :which-key "Org agenda")
  
  ;; Files
  "ff" '(counsel-rg :which-key "Ripgrep")
  "fr"  '(counsel-recentf :which-key "Recent Files")

  ;; Buffers
  "bb"  '(counsel-switch-buffer :which-key "Switch Buffer")
  "bd"  '(evil-delete-buffer :which-key "Delete Buffer")
  "bn"  '(evil-next-buffer :which-key "Next Buffer")
  "bp"  '(evil-prev-buffer :which-key "Next Buffer")
  "bN"  '(evil-buffer-new :which-key "New Buffer")

  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")

  ;; Others
  "at"  '(ansi-term :which-key "open terminal")
))

;; Increase the number of recent files that are stored
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 50)

;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

(setq projectile-completion-system 'ivy)

;; OrgMode Configs

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c@)")))

;; See: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
;; Adds a custom agenda view that displays all TODOs along with their place in time
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

;; Define Global Orgmode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;; Have Org notes logged into the LOGBOOK
(setq org-log-into-drawer t)

;; Improve org-refile across files
;;
;;   See: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;;
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-goto-interface 'outline-path-completion)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)


;; Make jumping to Org file headings fuzzy searchable using org-goto
;;
;;  See: https://emacs.stackexchange.com/questions/32617/how-to-jump-directly-to-an-org-headline
;;
(setq org-goto-interface 'outline-path-completion)
(setq org-outline-path-complete-in-steps nil)

;; Org Exports
(use-package ox-twbs
  :ensure t)

(use-package ox-reveal
  :load-path ("~/src/org-reveal")
  :defer 3
  :after org
  )

(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-return-follows-link t)
(setq org-log-done 'time)
(setq org-html-validation-link nil)

;; set line wrap for text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Define default capture file
(setq org-default-notes-file "~/org/capture.org")

;; Define capture templates
(setq org-capture-templates
	'(

	("b" "Templates for Ben")
	("bt" "New Task" entry (file+headline "~/Documents/benjamin/life.org" "Tasks")
	    "* TODO %?\n  %i\n  %a")

	("p" "Templates for PassionsPlay")
	("pt" "New Task" entry (file+headline "~/Documents/passionsplay/passionsplay.org" "Tasks")
	    "* TODO %?\n  %i\n  %a")

	("f" "Templates for FW")
	("ft" "New Task" entry(file+headline "~/Documents/flywheel/fw.org" "Tasks")
	    "* TODO %?\n %i\n %a")
	("fc" "Calendar" entry(file+headline "~/Documents/flywheel/fw.org" "Calendar")
	    "* %^{DateTime}T %?\n")

	("l" "Local")
	("lt" "New Task" entry(file+headline "~/Documents/local/local.org" "Tasks")
	    "* TODO %?\n %i\n %a")
	("lc" "Calendar" entry(file+headline "~/Documents/local/local.org" "Calendar")
	    "* %^{DateTime}T %?\n")

	))

(setq org-agenda-window-setup (quote only-window))

(eval-after-load 'org-agenda
 '(progn
    (evil-set-initial-state 'org-agenda-mode 'normal)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "<RET>") 'org-agenda-switch-to
      (kbd "\t") 'org-agenda-goto

      "q" 'org-agenda-quit
      "r" 'org-agenda-redo
      "S" 'org-save-all-org-buffers
      "gj" 'org-agenda-goto-date
      "gJ" 'org-agenda-clock-goto
      "gm" 'org-agenda-bulk-mark
      "go" 'org-agenda-open-link
      "s" 'org-agenda-schedule
      "+" 'org-agenda-priority-up
      "," 'org-agenda-priority
      "-" 'org-agenda-priority-down
      "y" 'org-agenda-todo-yesterday
      "n" 'org-agenda-add-note
      "t" 'org-agenda-todo
      ":" 'org-agenda-set-tags
      ";" 'org-timer-set-timer
      "i" 'org-agenda-clock-in-avy
      "O" 'org-agenda-clock-out-avy
      "u" 'org-agenda-bulk-unmark
      "x" 'org-agenda-exit
      "j"  'org-agenda-next-line
      "k"  'org-agenda-previous-line
      "vt" 'org-agenda-toggle-time-grid
      "va" 'org-agenda-archives-mode
      "vw" 'org-agenda-week-view
      "vl" 'org-agenda-log-mode
      "vd" 'org-agenda-day-view
      "vc" 'org-agenda-show-clocking-issues
      "g/" 'org-agenda-filter-by-tag
      "o" 'delete-other-windows
      "gh" 'org-agenda-holiday
      "gv" 'org-agenda-view-mode-dispatch
      "f" 'org-agenda-later
      "b" 'org-agenda-earlier
      "e" 'org-agenda-set-effort
      "n" nil  ; evil-search-next
      "{" 'org-agenda-manipulate-query-add-re
      "}" 'org-agenda-manipulate-query-subtract-re
      "A" 'org-agenda-toggle-archive-tag
      "." 'org-agenda-goto-today
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      "<" 'org-agenda-filter-by-category
      ">" 'org-agenda-date-prompt
      "F" 'org-agenda-follow-mode
      "D" 'org-agenda-deadline
      "H" 'org-agenda-holidays
      "J" 'org-agenda-next-date-line
      "K" 'org-agenda-previous-date-line
      "L" 'org-agenda-recenter
      "P" 'org-agenda-show-priority
      "R" 'org-agenda-clockreport-mode
      "Z" 'org-agenda-sunrise-sunset
      "T" 'org-agenda-show-tags
      "X" 'org-agenda-clock-cancel
      "[" 'org-agenda-manipulate-query-add
      "g\\" 'org-agenda-filter-by-tag-refine
      "]" 'org-agenda-manipulate-query-subtract)))

;; org2blog
(use-package org2blog
  :ensure t)

(setq org2blog/wp-use-sourcecode-shortcode t)
(setq org2blog/wp-sourcecode-langs
    '("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
	"erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
	"objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql" "vb"
	"xml" "sh" "emacs-lisp" "lisp" "lua"))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; SysAdmin
(use-package restclient
  :ensure t)

;; Programming
;; Smartparens
(use-package smartparens
  :ensure t)

;; Web Mode
(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Documents" )))
 '(package-selected-packages (quote (neotree general which-key helm evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
