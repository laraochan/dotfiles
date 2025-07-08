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

(use-package tramp
  :config
  (add-to-list 'tramp-remote-path "/home/larao/.nvm/versions/node/v22.17.0/bin"))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t)

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
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
	  (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc" "master" "src")
	  (html "https://github.com/tree-sitter/tree-sitter-html" "master" "src")
	  (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
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
(use-package php-ts-mode
  :mode (("\\.php\\'" . php-ts-mode)))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
	       '(php-ts-mode . ("intelephense" "--stdio")))
  :hook
  (typescript-ts-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (php-ts-mode . eglot-ensure))

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

(setq tramp-remote-path
      (append tramp-remote-path
              '("~/.nvm/versions/node/v22.17.0/bin"
                tramp-own-remote-path)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
