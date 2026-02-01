;;; init.el --- The laraochan's .emacs -*- coding: utf-8 ; lexical-binding: t -*-
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :custom '((user-full-name . "Sora Terao")
	    (user-mail-address . "me@larao.dev")
	    (user-login-name . "laraochan")
	    (create-lockfiles . nil)
	    (tab-width . 4)
	    (debug-on-error . t)
	    (init-file-debug . t)
	    (frame-resize-pixelwise . t)
	    (enable-recursive-minibuffers . t)
		(history-length . 1000)
		(history-delete-duplicates . t)
		(scroll-preserve-screen-position . t)
		(scroll-conservatively . 100)
		(mouse-wheel-scroll-amount . '(1 ((control) . 5)))
		(ring-bell-function . 'ignore)
		(text-quoting-style . 'straight)
		(truncate-lines . t)
		(use-dialog-box . nil)
		(use-file-dialog . nil)
		(menu-bar-mode . t)
		(tool-bar-mode . nil)
		(scroll-bar-mode . nil)
		(indent-tabs-mode . nil)))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :global-minor-mode show-paren-mode)

(leaf savehist
  :init (savehist-mode))

(leaf which-key
  :global-minor-mode t)

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-winter-is-coming-dark-blue t))

(leaf doom-modeline
  :global-minor-mode t
  :ensure t)

(leaf nyan-mode
  :global-minor-mode t
  :ensure t
  :custom
  (nyan-animate-nyancat . t))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content . t))

(leaf vertico
  :global-minor-mode t
  :ensure t)

(leaf marginalia
  :global-minor-mode t
  :ensure t)

(leaf nerd-icons
  :ensure t
  :config
  (leaf nerd-icons-dired
    :ensure t
    :hook (dired-mode-hook . nerd-icons-dired-mode))
  (leaf nerd-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))
  (leaf nerd-icons-completion
    :ensure t)
  (leaf nerd-icons-corfu
    :ensure t
    :after corfu
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(leaf corfu
  :ensure t
  :global-minor-mode (global-corfu-mode corfu-popupinfo-mode)
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0.1)
  (corfu-popupinfo-delay . 0.1)
  (corfu-auto-prefix . 2)
  (corfu-cycle . t)
  (corfu-quit-no-match . t)
  (corfu-quit-at-boundary . t))

(leaf cape
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard . t)) ;; Emacs 31: partial-completion behaves like substring

(leaf treesit
  :custom
  (treesit-font-lock-level . 4)
  (treesit-language-source-alist . '((json "https://github.com/tree-sitter/tree-sitter-json")
				     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
				     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
				     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
				     (html "https://github.com/tree-sitter/tree-sitter-html")
				     (css "https://github.com/tree-sitter/tree-sitter-css")
				     (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
				     (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
				     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")))
  :config
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
	  (message "treesit: %s is already installed" lang)
	(message "treesit: %s is not installed" lang)
	(treesit-install-language-grammar lang)))))

(leaf typescript-ts-mode
  :mode (("\\.[m]?ts\\'" . typescript-ts-mode)
	 ("\\.tsx\\'" . tsx-ts-mode)))

(leaf js-ts-mode
  :mode ("\\.[mc]?js\\'" . js-ts-mode))

(leaf json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode))

(leaf toml-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode))

(leaf php-ts-mode
  :mode ("\\.php\\'" . php-ts-mode))

(leaf eglot
  :hook ((tsx-ts-mode-hook . eglot-ensure)
	 (typescript-ts-mode-hook . eglot-ensure)
	 (php-ts-mode-hook . eglot-ensure)))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf magit
  :ensure t)

(leaf diff-hl
  :global-minor-mode t
  :ensure t)
