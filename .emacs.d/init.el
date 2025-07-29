;; <leaf-install-code>
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
;; </leaf-install-code>

(leaf emacs
  :config
  (set-frame-font "Hack Nerd Font Mono 16" nil t)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (which-key-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil))

(leaf vterm
  :ensure t)

(leaf treesit
  :config
  (setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
        )))

(leaf cyberpunk-theme
  :ensure t
  :config
  (load-theme 'cyberpunk t))

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf corfu
  :ensure t
  :custom
  (corfu-auto . t)
  (corfu-quit-no-match . 'separator)
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode)

(leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(leaf diff-hl
  :ensure t
  :global-minor-mode global-diff-hl-mode)

(leaf vertico
  :ensure t
  :global-minor-mode t)

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf projectile
  :ensure t
  :config
  (projectile-mode +1)
  (when (executable-find "ghq")
  (setq projectile-known-projects
        (mapcar
         (lambda (x) (abbreviate-file-name x))
         (split-string (shell-command-to-string "ghq list --full-path")))))
  :bind-keymap (("s-p" . 'projectile-command-map)))

(leaf rust-ts-mode
  :mode "\\.rs\\'")

(leaf php-ts-mode
  :mode ("\\.php\\'"))

(leaf go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-mod-ts-mode))
  :config
  (setq go-ts-mode-indent-offset 4
	indent-tabs-mode t))

(leaf tsx-ts-mode
  :mode (("\\.ts[x]?\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . tsx-ts-mode)
         ("\\.js[x]?\\'" . tsx-ts-mode)
         ("\\.[mc]js\\'" . tsx-ts-mode)))

(leaf toml-ts-mode
  :mode ("\\.toml\\'"))

(leaf json-ts-mode
  :mode ("\\.json\\'"))

(leaf eglot
  :config
  (setq jsonrpc-event-hook nil
	eglot-events-buffer-size 0)
  :hook (
  (eglot-managed-mode-hook . (lambda () (eglot-inlay-hints-mode -1)))
  (rust-ts-mode-hook . eglot-ensure)
  (tsx-ts-mode-hook . eglot-ensure)
  (typescript-ts-mode-hook . eglot-ensure)))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(leaf flycheck
  :ensure t)

(leaf flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode +1))

;; TODO: setup org-mode
(leaf org)

(leaf magit
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
