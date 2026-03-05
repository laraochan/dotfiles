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
    (leaf el-get :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf-tree
  :doc "visualize leaf.el configurations as a tree"
  :ensure t)

(leaf leaf-convert
  :doc "convert Emacs configurations into leaf.el format"
  :ensure t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :hook (window-setup-hook . toggle-frame-maximized)
  :custom '((user-full-name . "Sora Terao")
	        (user-mail-address . "me@larao.dev")
	        (user-login-name . "laraochan")
	        (create-lockfiles . nil)
	        (tab-width . 4)
	        (debug-on-error . nil)
	        (init-file-debug . t)
	        (frame-resize-pixelwise . t)
	        (enable-recursive-minibuffers . t)
	        (history-length . 1000)
	        (history-delete-duplicates . t)
	        (scroll-preserve-screen-position . t)
	        (scroll-conservatively . 100)
	        (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
	        (ring-bell-function .  'ignore)
	        (text-quoting-style . 'straight)
	        (truncate-lines . t)
	        (use-dialog-box . nil)
	        (use-file-dialog . nil)
	        (menu-bar-mode . t)
	        (tool-bar-mode . t)
	        (scroll-bar-mode . t)
	        (indent-tabs-mode . nil)))

(leaf exec-path-from-shell
  :doc "Make Emacs use the $PATH set up by the user's shell"
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf which-key
  :doc "Display available keybindings in popup"
  :global-minor-mode t)

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

(leaf paren
  :doc "highlight matching paren"
  :global-minor-mode show-paren-mode)

(leaf startup
  :doc "process Emacs shell arguments"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf delsel
  :doc "delete selection if you insert"
  :global-minor-mode delete-selection-mode)

(leaf savehist
  :doc "Save minibuffer history"
  :custom `((savehist-file . ,(locate-user-emacs-file "savehist")))
  :global-minor-mode t)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :global-minor-mode global-auto-revert-mode)

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t)

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)

(leaf nerd-icons
  :doc "icon library used across Emacs UI integrations"
  :ensure t
  :config
  (leaf nerd-icons-corfu
    :doc "add nerd-icons to Corfu completion margins"
    :ensure t
    :after "corfu"
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (leaf nerd-icons-dired
    :doc "display nerd-icons in Dired buffers"
    :ensure t
    :hook (dired-mode-hook . nerd-icons-dired-mode))
  (leaf nerd-icons-completion
    :doc "add nerd-icons to completion annotations"
    :ensure t
    :hook ((marginalia-mode-hook . nerd-icons-completion-marginalia-setup)))
  (leaf nerd-icons-ibuffer
    :doc "display nerd-icons in Ibuffer"
    :ensure t
    :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)))

(leaf vterm
  :doc "a fast terminal emulator inside Emacs"
  :ensure t
  :preface
  (defun my-project-shell ()
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
  (advice-add 'project-shell :override #'my-project-shell))

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :global-minor-mode global-corfu-mode corfu-popupinfo-mode
  :custom ((corfu-cycle . t)
	       (corfu-quit-at-boundary . t)
	       (corfu-quit-no-match . t)
	       (corfu-preview-current . t)
	       (corfu-auto . t)
	       (corfu-auto-delay . 0.2)))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :ensure t
  :custom ((completion-styles . '(orderless))
           (completion-category-defaults . nil)
           (completion-category-overrides . '((file (styles partial-completion))))))

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(leaf eglot
  :doc "Parentheses Universalistic"
  :ensure t
  :hook (tsx-ts-mode-hook . eglot-ensure))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf treesit
  :doc "built-in Tree-sitter integration for Emacs"
  :custom ((treesit-font-lock-level . 4)
		   (treesit-language-source-alist . '((json "https://github.com/tree-sitter/tree-sitter-json")
											  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
											  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))
  :config
  (dolist (element treesit-language-source-alist)
	(let* ((lang (car element)))
	  (if (treesit-language-available-p lang)
		  (message "treesit: %s is already installed" lang)
		(message "treesit: %s is not installed" lang)
		(treesit-install-language-grammar lang)))))

(leaf tsx-ts-mode
  :doc "major mode configuration for TypeScript/TSX/JavaScript using tree-sitter"
  :mode "\\.ts[x]?\\'" "\\.[m]ts\\'" "\\.js[x]?\\'" "\\.[mc]js\\'"
  :custom ((typescript-ts-mode-indent-offset . 2)))

(leaf json-ts-mode
  :doc "major mode configuration for JSON using tree-sitter"
  :mode "\\.json\\'")

(leaf magit
  :doc "A Git Porcelain inside Emacs"
  :ensure t)

(leaf diff-hl
  :doc "highlight uncommitted changes in the fringe/margin"
  :ensure t
  :global-minor-mode global-diff-hl-mode
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom ((diff-hl-disable-on-remote . t)))

(leaf doom-themes
  :doc "A megapack of themes for GNU Emacs"
  :ensure t
  :config
  (load-theme 'doom-solarized-dark t))

(leaf dashboard
  :doc "startup dashboard screen with recent items and shortcuts"
  :ensure t
  ;; TODO: Set banner (dashboard-startup-banner . [hoge.jpg])
  :custom ((dashboard-banner-logo-title . "Welcome to Emacs Dashboard")
           (dashboard-center-content . t)
           (dashboard-vertically-center-content . t)
           (dashboard-navigation-cycle . t)
           (dashboard-display-icons-p . t)
           (dashboard-icon-type . 'nerd-icons)
           (dashboard-set-heading-icons . t)
           (dashboard-set-file-icons . t))
  :config
  (dashboard-setup-startup-hook))

;; TODO: Set custom variable
(leaf doom-modeline
  :doc "modern and information-rich mode-line"
  :ensure t
  :global-minor-mode t)

(leaf agent-shell
  :doc "A native Emacs buffer to interact with LLM agents powered by ACP"
  :ensure t)
