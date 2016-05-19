(require 'cl-lib)

;;;; el-get
;; Used to require packages from places other than ELPA MELPA whatever

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer (url-retrieve-synchronously
			"https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; Use this to install external packages
(setq el-get-packages
 '(el-get
   typo))

;;;; MELPA ELPA etc
;; Set up normal package stuff

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(defun require-package (p)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p)))

;; Bootstrap packages

(package-initialize)
(el-get 'sync el-get-packages)

;;;; General

;; Customization settings in another file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; Highlight matching parens
(show-paren-mode 1)
;; For line wrap
(require-package 'visual-fill-column)
;; Tab control
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence tab-width 100 tab-width))
(defvaralias 'standard-indent 'tab-width)
(defvaralias 'c-basic-offset 'tab-width)
;; Backups
(setq backup-directory-alist '(("." . "~/.emacs/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;;; Dired+

(require-package 'dired+)
;; Reuse single dired buffer
(diredp-toggle-find-file-reuse-dir 1)

;;;; Magit

(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;; Org Mode

(setq org-fontify-whole-heading-line t)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

;;;; Markdown

(require-package 'markdown-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook (lambda () (setq fill-column 80)))

;;;; Spellcheck

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; Text-ish modes

(setq-default typo-language 'English)
(add-hook 'text-mode-hook
          (lambda ()
            (if (not (derived-mode-p 'html-mode 'yaml-mode))
                (typo-mode))))

;;;; Helm

(require-package 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; F that no tab completion nonsense
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;;;; Projectile

(require-package 'projectile)
(projectile-global-mode)
(require-package 'helm-projectile)
(helm-projectile-on)

;;;; Paredit

(require-package 'paredit)
(autoload 'enable-paredit-mode "paredit")
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'rust-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)

;; Powerline

(require-package 'powerline)
(powerline-vim-theme)

;; Clojure

(if (display-graphic-p) ;; if GUI
    (progn
      (require-package 'solarized-theme)
      (load-theme 'solarized-dark)))

;;;; Company Mode completion
;; Code completion

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;; Flycheck

(require-package 'flycheck)
(require-package 'flycheck-tip)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
(require-package 'flycheck-color-mode-line)
(with-eval-after-load "flycheck"
                 (set-face-attribute 'flycheck-error nil :background "red" :foreground "white")
                 (set-face-attribute 'flycheck-warning nil :background "yellow" :foreground "white"))

(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(add-hook 'clojure-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'yaml-mode 'flycheck-mode)
(add-hook 'javascript-mode 'flycheck-mode)

;;;; Clojure

(require-package 'clojure-mode)
(require-package 'cider)

;;;; C / C++

(require-package 'cc-mode)

;;;; Rust

(require-package 'rust-mode)
(require-package 'flycheck-rust)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook (lambda () (setq tab-width 0)))

;;;; Haskell

(require-package 'haskell-mode)

;;;; Web stuff
;; web-mode for all kinda templating languages
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
;; Make syntax highlighting less shitty
(add-hook 'web-mode-hook
          (lambda ()
            (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
            ;; Reload local variables
            (hack-local-variables)))

;;;; Node / JS

(defvaralias 'js-indent-level 'tab-width)
(require-package 'nvm)
(load "nvm") ;; Why do I have to do this?
(require-package 'js-comint)
(require-package 'js2-mode)
(setq js2-strict-missing-semi-warning nil)
(setq js2-bounce-indent-p t)
(setq js2-include-node-externs t)
(add-hook 'javascript-mode-hook 'electric-pair-mode)
(add-hook 'js2-mode-hook 'electric-pair-mode)

(defun set-node-version (version)
  (interactive "sNode version: ")
  (nvm-use version))

;;;; YAML

(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;; Docker

(require-package 'docker)
(require-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
