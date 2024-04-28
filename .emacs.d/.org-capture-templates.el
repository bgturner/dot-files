;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

;; Org Capture Templates
(setq org-capture-templates
'(

("t" "Task")
("tl" "Local Task"
 entry(file+headline
       "~/org/local.org"
       "Tasks")
"
* TODO %?
  %i
  %a
")

("tw" "WPE Task"
 entry(file+headline
       "~/org/wpe.org"
       "Tasks")
"
* TODO %?
  %i
  %a
")

("j" "Journal Entry"
 entry(file+olp+datetree
       "~/org/journal.org")
"
* %U %^{Summary}
  %a
  %?
"
:time-prompt t
:tree-type week
)))
