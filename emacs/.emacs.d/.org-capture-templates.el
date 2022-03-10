;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; Org Capture Templates
(setq org-capture-templates
'(

  ("i" "Inbox Task"
   entry (file+headline
	  "~/Sync/org/inbox.org"
	  "Tasks")
   "* TODO %?\n  %i\n  %a")

("b" "Blocks")
("bf" "Forums"
    entry (file+olp+datetree
	    "~/syncthing/WPE/org/forums/forum-time.org"
	    "Working Forums")
    "* Forums\n  :PROPERTIES:\n  :unreads_start:\n  :unreads_end:\n  :new_start:\n  :new_end:\n  :likes:\n  :replies:\n  :solves:\n  :END:\n  %T\n** Direct Replies\n** New Topics\n** Unreads\n  %?"
    :time-prompt t
    :tree-type week)

("bd" "Documentation"
    entry (file+olp+datetree
	"~/syncthing/WPE/org/documentation/local-docs.org"
	"Working Docs")
    "* Docs\n  %T\n  %?"
    :time-prompt t
    :tree-type week)

("bl" "LWP Site"
 entry (file+olp+datetree
	"~/syncthing/WPE/org/localwp.com/localwpcom.org"
	"Working LWP Site")
"* LWP Site\n  %T\n  %?"
:time-prompt t
:tree-type week)


("p" "Planning")
("pd" "Daily"
 entry (file+olp+datetree "~/syncthing/WPE/org/journal.org")
"
* Standup :meetings:local:scrum:
  %(org-insert-time-stamp (org-read-date nil t \"%T\") nil nil nil nil \" 07:00-07:15\")
"
    :time-prompt t)


("l" "Local")
("lt" "New Task"
 entry(file+headline
       "~/syncthing/WPE/org/local/local.org"
       "Tasks")
"
* TODO %?
  %i
  %a
")

("lc" "Clock Entry")
("lct" "Twitter"
 entry (file+olp+datetree
	"~/syncthing/WPE/org/local/task-clocking/twitter.org")
"* %^C\n  %?"
:clock-in t
:clock-resume t
:tree-type week)

("lcs" "Slack Support"
 entry (file+olp+datetree
	"~/syncthing/WPE/org/local/task-clocking/slack-support.org")
"* %^C\n  %?"
:clock-in t
:clock-resume t
:tree-type week)

("ld" "Dev Blocks")
("ldp" "Pull Requests"
 entry (file+olp+datetree
	"~/syncthing/WPE/org/pull-requests.org"
	"Clocking")
"* LWP Site\n  %T\n  %?"
:time-prompt t
:tree-type week)
("ldr" "Pair Programming"
 entry (file+olp+datetree
	"~/syncthing/WPE/org/pair-programming.org" "Clocking")
"* %^{Pairing}\n  %?"
:clock-in t
:clock-resume t
:tree-type week)

("o" "Open Source Journal"
 entry (file+olp+datetree
	"~/Sync/oss/open-source-software.org"
	"Calendar")
"
* %^{Heading|OSS}
  %T
  %?
"
:time-prompt t)))
