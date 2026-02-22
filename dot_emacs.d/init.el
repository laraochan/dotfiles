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

(leaf cus-start
  :doc "define customization properties of builtins"
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

(leaf simple
  :doc "basic editing commands for Emacs"
  :custom (eval-expression-print-length . nil))

(leaf files
  :doc "file input and output commands for Emacs"
  :global-minor-mode auto-save-visited-mode
  :custom `((auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf savehist
  :doc "Save minibuffer history"
  :custom `((savehist-file . ,(locate-user-emacs-file "savehist")))
  :global-minor-mode t)

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :global-minor-mode t)

(leaf leaf-convert
  :ensure t)

(leaf leaf-tree
  :ensure t
  :custom ((imenu-list-size . 30)
	   (imenu-list-position . 'left)))

(leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor . 'frappe)
  :config
  (load-theme 'catppuccin :no-confirm))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-center-content . t))

(leaf corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :bind ((corfu-map
	  ("C-s" . corfu-insert-separator))))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf vertico
  :ensure t
  :init
  (vertico-mode))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode))

(leaf nyan-mode
  :ensure t
  :global-minor-mode t
  :custom (nyan-animate-nyancat . t))

(leaf magit
  :ensure t)

(leaf vterm
  :ensure t)

(leaf projectile
  :ensure t
  :global-minor-mode t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(leaf treesit
  :custom
  (treesit-font-lock-level . 4)
  (treesit-language-source-alist . '((json "https://github.com/tree-sitter/tree-sitter-json")
                                     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  :config  
  ;; Treesitがインストールされてない場合は自動でインストールする
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
          (message "treesit: %s is already installed" lang)
        (message "treesit: %s is not installed" lang)
        (treesit-install-language-grammar lang)))))

(leaf tsx-ts-mode
  :mode "\\.ts[x]?\\'" "\\.[m]ts\\'" "\\.js[x]?\\'" "\\.[mc]js\\'")

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :hook ((tsx-ts-mode-hook . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("tailwindcss-language-server" "--stdio"))))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf markdown-mode
  :ensure t)

(leaf flymake-biome
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((eval setq-local flymake-biome-program
           (expand-file-name "node_modules/@biomejs/biome/bin/biome"
                             (or
                              (locate-dominating-file
                               default-directory "package.json")
                              default-directory)))
     (eval flymake-biome-load) (eval require 'flymake-biome))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
