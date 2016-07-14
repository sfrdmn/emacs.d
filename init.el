(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(defun require-package (p)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p)))

;; General

(show-paren-mode 1)
(require-package 'visual-fill-column)

;; Org Mode

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

;; Spellcheck

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Paredit

(require-package 'paredit)
(autoload 'enable-paredit-mode "paredit")
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'rust-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Flycheck

(require-package 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'rust-mode 'flycheck-mode)

;; Solarized

(require-package 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-light)

;; Clojure

(require-package 'clojure-mode)
(require-package 'cider)

;; Smex

(require-package 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x

;; Company Mode completion

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck

(require-package 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)

;; C / C++

(require-package 'cc-mode)

;; Rust

(require-package 'rust-mode)
(require-package 'flycheck-rust)
