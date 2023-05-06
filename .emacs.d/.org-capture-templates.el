;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; Org Capture Templates
(setq org-capture-templates
'(

  ("i" "Inbox Task"
   entry (file+headline
	  "~/WPESync/inbox.org"
	  "Tasks")
   "* TODO %?\n  %i\n  %a")

  ("c" "Clocking")
  
  ("cj" "Career Journal"
   entry (file+olp+datetree
	  "~/WPESync/org/performance-reviews/career.org"
	  "Journal")
   "* %^{Career Improvement}\n  %T\n  %?"
   :time-prompt t
   :tree-type week)

  ("cw" "WP Contributor Day"
    entry (file+olp+datetree
	   "~/WPESync/org/oss/contributor-day.org"
	   "Calendar")
    "* Contributor Day\n  %T\n  %?"
    :time-prompt t
    :tree-type week)

  ("cg" "Goals")

  ("cgk" "Learn k8s"
   entry (file+olp+datetree
	  "~/WPESync/org/goals/2023-learn-k8s.org"
	  "Journal")
   "* %^{Summary}\n  :PROPERTIES:\n  :time_spent: %^{HH:MM}\n  :END:\n  %?"
   :time-prompt t
   :tree-type week)

  ("cgt" "Improve testing skills"
   entry (file+olp+datetree
	  "~/WPESync/org/goals/2023-improve-testing.org"
	  "Journal")
   "* %^{Summary}\n  :PROPERTIES:\n  :time_spent: %^{HH:MM}\n  :END:\n  %?"
   :time-prompt t
   :tree-type week)

  ("cgm" "Improve mentoring skills"
   entry (file+olp+datetree
	  "~/WPESync/org/goals/2023-improve-mentoring-skills.org"
	  "Journal")
   "* %^{Summary}\n  :PROPERTIES:\n  :time_spent: %^{HH:MM}\n  :END:\n  %?"
   :time-prompt t
   :tree-type week)

("b" "Blocks")

("bd" "Documentation"
    entry (file+olp+datetree
	"~/WPESync/org/documentation/local-docs.org"
	"Working Docs")
    "* Docs\n  %T\n  %?"
    :time-prompt t
    :tree-type week)

("bf" "Forums"
    entry (file+olp+datetree
	    "~/WPESync/org/forums/forum-time.org"
	    "Working Forums")
    "* Forums\n  %?"
    :time-prompt t
    :tree-type week)

("p" "Planning")
("pd" "Daily"
 entry (file+olp+datetree "~/WPESync/org/journal.org")
"
* Standup :meetings:local:scrum:
  %(org-insert-time-stamp (org-read-date nil t \"%T\") nil nil nil nil \" 07:00-07:15\")
"
    :time-prompt t)

("l" "Local")
("lt" "New Task"
 entry(file+headline
       "~/WPESync/org/local/local.org"
       "Tasks")
"
* TODO %?
  %i
  %a
")

("ld" "Dev Blocks")
("ldp" "Pull Requests"
 entry (file+olp+datetree
	"~/WPESync/org/pull-requests.org"
	"Clocking")
"* Pull Requests\n  %T\n  %?"
:time-prompt t
:tree-type week)

("ldr" "Pair Programming"
 entry (file+olp+datetree
	"~/WPESync/org/pair-programming.org" "Clocking")
"* %^{Pairing}\n  %?"
:clock-in t
:clock-resume t
:tree-type week)))
