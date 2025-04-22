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
