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

(load-theme 'leuven t)
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta))

(leaf nyan-mode
  :ensure t
  :config
  (nyan-mode))

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf company
  :ensure t
  :config
  (global-company-mode))

(leaf vterm
  :ensure t)
(leaf vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-scope . 'project)
  :bind ("C-c t" . 'vterm-toggle))

(leaf projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(leaf neotree
  :ensure t
  :bind ("C-c C-t" . 'neotree-project-dir))

(leaf which-key
  :ensure t
  :config
  (which-key-mode))

(leaf go-mode
  :ensure t)
(leaf typescript-mode
  :ensure t)
(leaf web-mode
  :ensure t)
(leaf lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (go-mode-hook . lsp)
	 (typescript-mode-hook . lsp)
	 (web-mode-hook . lsp)
	 )
  :commands lsp)
(leaf lsp-ui
  :ensure t)
(leaf flycheck
  :ensure t)

(leaf magit
  :ensure t)
(leaf git-gutter
  :ensure t)

;; NeoTree can be opened (toggled) at projectile project root
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
