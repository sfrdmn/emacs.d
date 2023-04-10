(when (version< emacs-version "30")
	(error "Configuration requires Emacs version >= 30"))

(add-to-list 'custom-theme-load-path
						 (expand-file-name "emacs-color-theme-solarized" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(when window-system
  (scroll-bar-mode -1)
  (load-theme 'solarized))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq backup-directory-alist
			'(("" . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq ring-bell-function 'ignore)
(setq-default tab-width 2)

(add-to-list 'treesit-extra-load-path
						 (expand-file-name user-emacs-directory "tree-sitter-module/dist"))

(use-package helm
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
	("C-x C-b" . helm-buffers-list)
	("C-x C-f" . helm-find-files)
	:map helm-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-z" . helm-select-action))
  :config (helm-mode))

(use-package go-ts-mode
  :mode "\\.go\\'")

(use-package go-mod-ts-mode
  :mode "\\.mod\\'")

(use-package javascript-ts-mode
  :mode "\\.js\\'")

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package eglot
  :hook (go-ts-mode . eglot-ensure)
  (go-mod-ts-mode . eglot-ensure)
	(javascript-ts-mode . eglot-ensure)
	(typescript-ts-mode . eglot-ensure)
	(tsx-ts-mode . eglot-ensure))
