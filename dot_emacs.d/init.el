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
    :config
    ;; initialize leaf-keywords.el
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
  :hook ((window-setup-hook . toggle-frame-maximized))
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

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :global-minor-mode t)

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf magit
  :doc "a complete Git porcelain inside Emacs"
  :ensure t)

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
		   (corfu-quit-at-boundary . nil)
		   (corfu-quit-no-match . t)
		   (corfu-preview-current . t)))

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file))

(leaf nerd-icons
  :ensure t
  :config
  (leaf nerd-icons-corfu
	:ensure t
	:after "corfu"
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (leaf nerd-icons-dired
    :ensure t
    :hook (dired-mode-hook . nerd-icons-dired-mode)))

(leaf eglot
  :doc "Parentheses Universalistic"
  :ensure t
  :hook ((tsx-ts-mode-hook . eglot-ensure)))

(leaf eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :global-minor-mode t)

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

(leaf treemacs
  :doc "a tree-style project and file explorer sidebar"
  :ensure t)

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

(leaf diff-hl
  :doc "highlight uncommitted changes in the fringe/margin"
  :ensure t
  :global-minor-mode global-diff-hl-mode
  :hook ((magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :custom ((diff-hl-disable-on-remote . t)))
