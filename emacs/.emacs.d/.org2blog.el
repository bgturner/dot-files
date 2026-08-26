;; org2blog
(use-package org2blog
  :bind (
	 ("C-c ; o b" . org2blog-user-interface)
	)
  :config
    (global-set-key (kbd "C-c o r d") 'org-refile-to-datetree)
    (setq org2blog/wp-show-post-in-browser t)
    (setq org2blog/wp-use-sourcecode-shortcode t)
    (setq org2blog/wp-sourcecode-langs
	'("actionscript3" "bash" "coldfusion" "cpp" "csharp" "css" "delphi"
	    "erlang" "fsharp" "diff" "groovy" "javascript" "java" "javafx" "matlab"
	    "objc" "perl" "php" "text" "powershell" "python" "ruby" "scala" "sql" "vb"
	    "xml" "sh" "emacs-lisp" "lisp" "lua"))

    ;; Blog config
    (setq org2blog/wp-blog-alist '(
	("viking-investor"
	    :url "http://vicom-clone.local/xmlrpc.php"
	    :username "benjamin"
	    :default-title "Hello World"
	    :default-categories ("org2blog" "emacs")
	    :tags-as-categories nil)
	("org2blog"
	    :url "http://org2blog.local/xmlrpc.php"
	    :username "org2blog"
	    :default-title "Codechallenge"
	    :default-categories ("org2blog" "emacs")
	    :tags-as-categories nil)
	("passionsplay"
	    :url "https://passionsplay.com/xmlrpc.php"
	    :username "benjamin"
	    :default-title "Hello World"
	    :default-categories ("development")
	    :tags-as-categories nil)
	("btkb"
	    :url "http://flywheel:three-move@precious-lager.flywheelsites.com/xmlrpc.php"
	    :username "benjamin"
	    :default-categories ("development")
	    :tags-as-categories nil)
	("yolo-local"
	    :url "http://yolo.local/xmlrpc.php"
	    :username "ben.turner"
	    :default-categories ("Local")
	    :tags-as-categories nil)
	))
    )
