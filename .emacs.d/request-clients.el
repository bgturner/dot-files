;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request Clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package restclient
  :defer t)

(use-package graphql-mode
  :defer t)

(use-package graphql
  :defer t)

(defun bt/restclient-sandbox ()
    "Create a new restclient sandbox to explore urls."
  (interactive)
  (let ((buffer (generate-new-buffer "restclient-sandbox.http")))
    (with-current-buffer buffer
      (restclient-mode)
      (insert "# restclient sandbox\n\n#\n"))
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))
