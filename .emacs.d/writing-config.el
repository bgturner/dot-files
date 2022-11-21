;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package writeroom-mode
  :defer t
  :config
    (define-key writeroom-mode-map (kbd "C-M-<") 'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C-M->") 'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-M-=") 'writeroom-adjust-width))

(use-package focus
  :defer t)

(use-package wc-mode
  :init
    (setq wc-modeline-format "WC[%W%w/%tw:%gw]"))

(use-package define-word
  :defer t
  :straight (:host github :repo "abo-abo/define-word"))

(use-package clear-text
  :defer t
  :straight (:host github :repo "xuchunyang/clear-text.el"))

(use-package flyspell
  :defer t
  :delight
  :init
    (defun flyspell-check-next-highlighted-word ()
	"Custom function to spell check next highlighted word"
	(interactive)
	(flyspell-goto-next-error)
	(ispell-word))
  :config
    (global-set-key (kbd "C-c s m") 'flyspell-mode)
    (global-set-key (kbd "C-c s b") 'flyspell-buffer)
    (global-set-key (kbd "C-c s w") 'ispell-word)
    (global-set-key (kbd "C-c s c") 'flyspell-check-next-highlighted-word)
    (when (executable-find "hunspell")
	(setq-default ispell-program-name "hunspell")
	(setq ispell-really-hunspell t))
  :hook
    (text-mode . flyspell-mode))

;; Edit Chrome textareas in Emacs
(use-package edit-server
  :init
    (setq edit-server-new-frame nil) ;; Open in new buffer of existing frame
  ;; Start the server and use markdown by default
  :config
    (edit-server-start)
    (add-hook 'edit-server-start-hook 'markdown-mode))
