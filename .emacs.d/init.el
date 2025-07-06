(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(which-key-mode +1)
(global-display-line-numbers-mode +1)
(electric-pair-mode +1)
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

(use-package org
  :ensure t
  :init
  (setq org-directory "~/org"
        org-daily-tasks-file (format "%s/tasks.org" org-directory)
	org-capture-templates '(("d" "daily" entry (file org-daily-tasks-file) "%[~/org/templates/daily.org]" :empty-lines-before 1 :prepend t))
	org-startup-folded 'overview)
  :config
  (global-set-key (kbd "C-c c n") #'org-capture)
  (global-set-key (kbd "C-c c o") (lambda () (interactive)
				    (find-file "~/org/tasks.org"))))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-switch-project-action
      (lambda ()
        (eshell)))
  (when (executable-find "ghq")
  (setq projectile-known-projects
        (mapcar
         (lambda (x) (abbreviate-file-name x))
         (split-string (shell-command-to-string "ghq list --full-path"))))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (setq corfu-auto t
	corfu-quit-no-match 'separator
	corfu-cycle t
	corfu-popupinfo-delay 1.0))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package treesit
  :init
  (setq treesit-language-source-alist
	'((go "https://github.com/tree-sitter/tree-sitter-go" "master" "src")
	  (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
	  ))
  :config
  (setq treesit-font-lock-level 4))

(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-mod-ts-mode))
  :config
  (setq go-ts-mode-indent-offset 4
	indent-tabs-mode t))
(use-package tsx-ts-mode
  :mode (("\\.ts[x]?\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . tsx-ts-mode)
         ("\\.js[x]?\\'" . tsx-ts-mode)
         ("\\.[mc]js\\'" . tsx-ts-mode)))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
	       '(php-ts-mode . ("intelephense" "--stdio")))
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure))

(use-package flycheck
  :ensure t)

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode +1))

(use-package flycheck-phpstan
  :ensure t)

(use-package magit
  :ensure t)

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
