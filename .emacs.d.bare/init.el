(setq user-emacs-directory "~/.emacs.d.bare")
(setq custom-file "~/.emacs.d.bare/custom.el")
(load custom-file)

(setq use-package-always-ensure t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(global-set-key [remap list-buffers] 'ibuffer)

(use-package emacs
  :init
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq inhibit-startup-screen t)
  :config
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  (line-number-mode +1)
  (column-number-mode t)
  (size-indication-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load-theme 'misterioso))

(use-package evil
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1)
  (evil-define-key 'normal 'global "]q" 'next-error)
  (evil-define-key 'normal 'global "[q" 'previous-error))

