;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.
The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file. A datetree within a subheading
is possible if the heading has a property of DATE_TREE."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (if file (find-file file))
	(widen)
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (newline)
        (goto-char (point-max))
        (org-paste-subtree (+ org-datetree-base-level 3))
        (widen)
        ))))

(use-package org
  :init
    ; General Settings
    (setq org-hide-emphasis-markers t
	  org-clock-clocked-in-display nil
	  org-tags-column 0 ; tags are right after headline
	  org-hide-leading-stars t
	  org-adapt-indentation t
	  org-confirm-babel-evaluate nil
	  org-log-into-drawer t
	  org-log-done 'time
	  org-html-validation-link nil
	  org-export-with-section-numbers nil
	  org-export-with-toc nil
	  org-export-with-author nil
	  org-export-with-email nil
	  org-export-with-date nil
	  org-priority-default 67 ;; Have the default priority be "C"
	  org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@)"))
          org-columns-default-format "%60ITEM(Task) %TODO %6Effort(Estim){:}  %6CLOCKSUM(Clock){:} %TIMESTAMP %SCHEDULED(Scheduled) %DEADLINE(Deadline)"
          org-global-properties '(("Effort_ALL" . "0 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))
	  org-capture-templates '(("i" "Inbox Task" entry (file+headline "~/WPESync/inbox.org" "Tasks")
				   "* TODO %?\n  %i\n  %a")))

    ; Agenda Settings
    (setq org-agenda-span 'day
	   org-agenda-todo-ignore-scheduled 'all
	   org-agenda-entry-text-maxlines 10
	   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :fileskip0 t :tags nil) ;; Clocktable in agenda view
	   org-agenda-skip-additional-timestamps-same-entry t
	   org-agenda-skip-scheduled-if-done t
	   org-agenda-skip-deadline-if-done t
	   org-agenda-sorting-strategy '((agenda time-up todo-state-down priority-down)
					 (todo priority-down category-up)
					 (tags priority-down category-keep)
					 (search category-keep))
	   org-agenda-use-time-grid t
	   org-agenda-time-grid '((daily today)
				  (800 1000 1200 1400 1600 1800 2000)
				  "......" "----------------"))

    ;; Improve org-refile across files
    ;;
    ;;   See: https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
    ;;
    (setq org-refile-targets '((org-agenda-files :maxlevel . 5))
          org-goto-interface 'outline-path-completion
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm)

    ;; Make jumping to Org file headings fuzzy searchable using org-goto
    ;;
    ;;  See: https://emacs.stackexchange.com/questions/32617/how-to-jump-directly-to-an-org-headline
    ;;
    (setq org-goto-interface 'outline-path-completion
          org-outline-path-complete-in-steps nil)

  :config
    (global-set-key (kbd "C-c o a") 'org-agenda)
    (global-set-key (kbd "C-c o c") 'org-capture)
    (global-set-key (kbd "C-c o b") 'org-switchb)
    (global-set-key (kbd "C-c o r d") 'org-refile-to-datetree)

    ;; View item from agenda in narrowed buffer.
    ;; Useful when using "follow" in agenda views.
    ;;     See: https://emacs.stackexchange.com/questions/17797/how-to-narrow-to-subtree-in-org-agenda-follow-mode
    ;;
    (advice-add 'org-agenda-goto :after
		(lambda (&rest args)
		  (org-narrow-to-subtree)))

    ;; Define babel languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
        (dot . t)
        (shell . t)
	(sql . t)
	(python . t)
	(css . t)
	(js . t)
       ))

    (load-user-file ".org-capture-templates.el")

    (bt/set-org-agenda-files)

) ;; End orgmode config

(eval-after-load "org"
  '(require 'ox-md nil t))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todos.org")
  (global-set-key (kbd "C-c o n p") 'org-projectile-project-todo-completing-read)
  (setq org-projectile-capture-template "* TODO %?\n  %a"))


(use-package org-mru-clock
  :ensure t
  :bind* (("C-c C-x i" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :config
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read)
  (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook))

(use-package org-habit-plus
  :defer t
  :straight (:host github :repo "myshevchuk/org-habit-plus")
  :init
  (setq org-habit-graph-column 60
      org-habit-preceding-days 10
      org-habit-show-habits-only-for-today nil
      org-habit-show-done-always-green t)
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit))

(use-package org-ql
  :straight (:host github :repo "alphapapa/org-ql"))

(require 'org-tempo) ; needed to make <s<tag> expand to src blocks in macos

(use-package org-web-tools)

(use-package ox-twbs)

(use-package ox-reveal
  :straight (:host github :repo "yjwen/org-reveal"))

(use-package org-journal
  :defer t
  :bind
    ("C-c n j" . org-journal-new-entry)
  :custom
    (org-journal-dir "~/Sync/org/roam/journal/")
    (org-journal-file-format "journal-%Y-%m-%d.org")
    (org-journal-date-prefix "#+CATEGORY: journal\n#+TITLE: Journal - ")
    (org-journal-date-format "%Y-%m-%d - %A"))

(defun org-journal-find-location ()
  "Open today's journal. Specify a non-nil prefix in order to inhibit
inserting the heading which will be handled by 'org-capture'."
    (org-journal-new-entry t)
    (goto-char (point-max)))

(add-to-list 'org-capture-templates
	     '("j" "Journal" plain (function org-journal-find-location)
	       "* %^{Title}\n%U\n%i\n%? "
	       :empty-lines 1
	       :jump-to-captured t))

(use-package org-roam
      :init
      (setq org-roam-v2-ack t)
      :ensure t
      :custom
      (org-roam-directory (file-truename "~/OrgRoam"))
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture))
      :config
      (org-roam-db-autosync-mode)
      ;; If using org-roam-protocol
      ;; (require 'org-roam-protocol)
      )

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package ob-typescript)
