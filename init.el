(when (version< emacs-version "29")
  (error "Configuration requires Emacs version >= 29"))

(require 'package)
(require 'project)

(add-to-list 'custom-theme-load-path
						 (expand-file-name "emacs-color-theme-solarized" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(setq-default indent-tabs-mode nil)

(when (file-exists-p custom-file)
  (load custom-file))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq backup-directory-alist
			'(("" . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-dir (locate-user-emacs-file "auto-saves"))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "\\1" auto-save-dir) t)))

(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "\\1" auto-save-dir) t)))

(setq ring-bell-function 'ignore)
(setq-default tab-width 2)
(global-auto-revert-mode t)

(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter-module/dist")

(when window-system
  (scroll-bar-mode -1)
  (setq frame-background-mode 'dark)
  (add-hook 'after-init-hook (lambda () (load-theme 'solarized)))

  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(use-package magit
  :ensure t
  :commands magit)

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
	("C-x C-b" . helm-buffers-list)
	("C-x C-f" . helm-find-files)
	:map helm-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-z" . helm-select-action))
  :config (helm-mode))

(use-package helm-ag
  :ensure t
	:bind 	("C-x p s" . helm-do-ag-project-root)
  :config
  (setq helm-ag-use-grep-ignore-list 't)
  (setq helm-ag-use-agignore 't))

(use-package dockerfile-ts-mode
    :mode "Dockerfile.*\\'")

(use-package go-ts-mode
  :init
  (add-hook 'project-find-functions #'project-find-go-module)
	:config
	(setq indent-tabs-mode 1)
	(setq go-ts-mode-indent-offset 2))

(use-package js-ts-mode
  :mode "\\.js\\'")

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :mode "\\.tsx\\'")

(use-package eglot
  :hook
	(go-ts-mode . eglot-ensure)
	(javascript-ts-mode . eglot-ensure)
	(typescript-ts-mode . eglot-ensure)
	(tsx-ts-mode . eglot-ensure))

(use-package company
  :ensure t
  :after eglot
  :hook
  (eglot-managed-mode . global-company-mode))

(use-package prettier
  :ensure t
  :hook (after-init . global-prettier-mode)
  :config
  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-mode typescript babel-ts))
  (add-to-list 'prettier-major-mode-parsers '(tsx-ts-mode typescript babel-ts)))
