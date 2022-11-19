;; Ivy: Generic completion framework
(use-package ivy
  :delight
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (bind-key "C-c C-r" 'ivy-resume)
  (ivy-mode 1))

;; Ivy enhanced version of Isearch
(use-package swiper
  :delight
  :config
  (global-set-key "\C-s" 'swiper))

;; Counsel: Collection of Ivy-enhanced common Emacs commands.
(use-package counsel
  :delight
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x j m") 'counsel-evil-marks)
  (global-set-key (kbd "C-x j r") 'counsel-evil-registers)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :custom
  (ivy-height 20 "number of result lines to display"))

;; Transform Ivy display items to have more information.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
