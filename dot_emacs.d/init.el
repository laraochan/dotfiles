(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-material t))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package nerd-icons
  :straight t)

(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2))

(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package orderless
  :straight t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  (completion-ignore-case t))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package treesit
  :init
  (setq treesit-language-source-alist
	'((json "https://github.com/tree-sitter/tree-sitter-json")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")))
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
	  (message "treesit: %s is already installed" lang)
	(message "treesit: %s is not installed" lang)
	(treesit-install-language-grammar lang))))
  :custom
  (treesit-font-lock-level 4))

(use-package json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode))

(use-package typescript-ts-mode
  :mode (("\\.ts[x]?\\'" . tsx-ts-mode)
	 ("\\.[m]ts\\'" . tsx-ts-mode)
	 ("\\.js[x]?\\'" . tsx-ts-mode)
	 ("\\.[mc]js\\'" . tsx-ts-mode)))

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode))

(use-package eglot
  :hook ((tsx-ts-mode . eglot-ensure)
	 (rust-ts-mode . eglot-ensure)))

(use-package agent-shell
  :straight t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
