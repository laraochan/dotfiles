;; Setup use-package
(require 'package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta))

(load-theme 'leuven t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(use-package nyan-mode
  :config
  (nyan-mode)
  (setq nyan-animate-nyancat t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))


(use-package go-mode
  :ensure t
  :config
  (setq tab-width 2))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (go-mode . lsp)
	 )
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-file-watchers nil))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package magit)

(use-package vterm)

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (use-package ripgrep))

(use-package which-key
  :config
  (which-key-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
