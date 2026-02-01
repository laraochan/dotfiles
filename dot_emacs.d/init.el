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
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-moonlight t))

(leaf vertico
  :ensure t
  :global-minor-mode t)

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf treesit
  :custom
  (treesit-font-lock-level . 4)
  :config
  (leaf treesit-auto
    :ensure t
    :global-minor-mode t))

(leaf typescript-ts-mode
  :mode '(("\\.[mc]?ts\\'" . typescript-ts-mode)
		  ("\\.tsx\\'" . tsx-ts-mode)))

(leaf eglot
  :hook ((typescript-ts-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . eglot-ensure)))

(leaf magit
  :ensure t)

(leaf vterm
  :ensure t
  :config
  (leaf multi-vterm
	:ensure t
	:after vterm))

(leaf nerd-icons
  :ensure t
  :config
  (leaf nerd-icons-dired
    :ensure t
    :after dired
    :hook (dired-mode-hook . nerd-icons-dired-mode))
  (leaf nerd-icons-ibuffer
    :ensure t
    :after ibuffer
    :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))
  (leaf nerd-icons-completion
    :ensure t
    :after marginalia
    :global-minor-mode t
    :hook (marginalia-mode-hook . #'nerd-icons-completion-marginalia-setup))
  (leaf nerd-icons-corfu
    :ensure t
    :after corfu
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))


(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner . "~/.emacs.d/assets/127051174_p0.jpg")
  (dashboard-image-banner-max-width . 300)
  (dashboard-image-banner-max-height . 300)
  (dashboard-center-content . t)
  (dashboard-display-icons-p . t)
  (dashboard-icon-type . 'nerd-icons))

(leaf doom-modeline
  :ensure t
  :global-minor-mode t)

(leaf nyan-mode
  :ensure t
  :global-minor-mode t
  :custom
  (nyan-animate-nyancat . t))

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

(leaf which-key
  :global-minor-mode t)

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :custom
  (user-full-name . "Sora Terao")
  (user-mail-address . "me@larao.dev")
  (user-login-name . "laraochan")
  (create-lockfiles . nil)
  (tab-width . 4)
  (indent-tab-mode . nil)
  (debug-on-error . t)
  (init-file-debug . t)
  (frame-resize-pixelwise . t)
  (enable-recursive-minibuffers . t)
  (history-length . 1000)
  (history-delete-deplicates . t)
  (scroll-preserve-screen-position . t)
  (scroll-conservatively . 100)
  (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
  (ring-bell-function . 'ignore)
  (text-quoting-style . 'straight)
  (truncate-lines . t)
  (use-dialog-box . nil)
  (use-file-dialog . nil)
  (menu-bar-mode . t)
  (tool-bar-mode . t)
  (scroll-bar-mode . nil))

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

(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :bind ((prog-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error)))
  :config
  (leaf flymake-eslint
    :ensure t)
  (leaf phpstan
    :ensure t))

(leaf markdown-mode
  :ensure t)

(leaf org
  :config
  (leaf org-modern
	:ensure t
	:hook ((org-mode-hook . org-modern-mode)
		   (org-agenda-finalize-hook . org-modern-mode))))
