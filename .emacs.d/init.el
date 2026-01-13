;; -*- lexical-binding: t -*-
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

(leaf paren
  :doc "highlight matching paren"
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :custom ((kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf files
  :doc "file input and output commands for Emacs"
  :global-minor-mode auto-save-visited-mode
  :custom `((auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)
            (auto-save-visited-interval . 1)))

(leaf startup
  :doc "process Emacs shell arguments"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf savehist
  :doc "Save minibuffer history"
  :custom `((savehist-file . ,(locate-user-emacs-file "savehist")))
  :global-minor-mode t)

(leaf which-key
  :doc "Display available keybindings in popup"
  :global-minor-mode t)

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night t))

(leaf corfu
  :ensure t
  :global-minor-mode (global-corfu-mode corfu-popupinfo-mode)
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0.2)
  (corfu-auto-prefix . 1)
  (corfu-quit-no-match . t)
  (corfu-cycle . t))

(leaf kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(leaf eglot
  :doc "The Emacs Client for LSP servers"
  :hook ((tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure))
  :custom ((eldoc-echo-area-use-multiline-p . nil)
           (eglot-connect-timeout . 600)))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc ( :url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf nyan-mode
  :ensure t
  :global-minor-mode t)


(leaf treesit
  :doc "tree-sitter utilities"
  :custom ((treesit-font-lock-level . 4)
           (treesit-language-source-alist . '((json "https://github.com/tree-sitter/tree-sitter-json")
                                              (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                              (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                              (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
                                              (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
                                              (toml "https://github.com/tree-sitter/tree-sitter-toml"))))
  :config
  (dolist (element treesit-language-source-alist)
    (let* ((lang (car element)))
      (if (treesit-language-available-p lang)
          (message "treesit: %s is already installed" lang)
        (message "treesit: %s is not installed" lang)
        (treesit-install-language-grammar lang)))))

(leaf typescript-ts-mode
  :doc "Major mode for editing TSX and JSX documents"
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.[m]ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.[mc]js\\'" . typescript-ts-mode)))

(leaf json-ts-mode
  :doc "Major mode for editing JSON, powered by tree-sitter"
  :mode ("\\.json\\'" . json-ts-mode))

(leaf php-ts-mode
  :doc "Major mode for editing PHP, powered by tree-sitter"
  :mode ("\\.php\\'" . php-ts-mode))

(leaf toml-ts-mode
  :doc "Major mode for editing TOML, powered by tree-sitter"
  :mode ("\\.toml\\'" . toml-ts-mode))

(leaf markdown-ts-mode
  :doc "Major mode for editing Markdown using tree-sitter grammar"
  :mode ("\\.md\\'" . markdown-ts-mode))

(leaf magit
  :doc "A Git Porcelain inside Emacs"
  :ensure t)

(leaf vterm
  :doc "Emacs-libvterm (vterm) is fully-fledged terminal emulator inside GNU Emacs based on libvterm, a C library"
  :ensure t
  :config
  (defun project-vterm ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (require 'comint)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "shell"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
            (vterm shell-buffer))
        (vterm (generate-new-buffer-name default-project-shell-name)))))
  (advice-add 'project-shell :override #'project-vterm))
