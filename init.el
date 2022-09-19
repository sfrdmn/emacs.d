;;;;;;;;;;;;;;;;;
;;;;; Utils ;;;;;
;;;;;;;;;;;;;;;;;

(defmacro add-many! (target &rest items)
  `(dolist (item (quote ,items)) (add-to-list (quote ,target) item)))

;;;;;;;;;;;;;;;;;;;;;
;;;;; Bootstrap ;;;;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; Try manually eval-ing this body when packages cannot be found
(if nil (package-refresh-contents))

(if (not (package-installed-p 'use-package))
  (package-install 'use-package)
  (require 'use-package))

(with-eval-after-load 'gnutls
  (if (eq system-type 'darwin)
      (add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Packages and Configuration ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package general-config
  :no-require t
  :config
  (global-unset-key (kbd "C-z")))

(use-package custom-var-config
  :no-require t
  :config
  (setq custom-file (concat user-emacs-directory "custom.el")))

(use-package prog-edit-config
  :no-require t
  :config
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default tab-stop-list (number-sequence tab-width 100 tab-width))
  (setq-default standard-indent tab-width)
  (setq-default c-basic-offset tab-width)
  (add-hook 'prog-mode-hook
            (progn (show-paren-mode 1))))

(use-package backup-config
  :no-require t
  :config
  (setq backup-directory-alist '(("" . "~/.emacs.d/backups"))
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))

(use-package window-config
  :no-require t
  :if window-system
  :config
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(use-package exec-path-from-shell
  :if window-system
  :ensure t
  :config (exec-path-from-shell-initialize))


(use-package solarized-theme
  :if window-system
  :ensure t
  :config (load-theme 'solarized-dark t))

(use-package whitespace
  :ensure t
  :config (setq whitespace-style '(trailing tabs tab-mark)))

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode
  :diminish rainbow-mode)

(use-package projectile
  :ensure t
  :demand t
  :diminish projectile-mode
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p" . projectile-command-map))
  :config
  (projectile-mode))

(use-package helm
  :ensure t
  :demand t
  :straight t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (helm-mode)
  (add-many! helm-boring-buffer-regexp-list
             "\\*GNU Emacs\\*"
             "\\*Help\\*"
             "\\*Messages\\*")
  ;; Omit Helm boring buffer from frame cycling
  (set-frame-parameter
   (selected-frame) 'buffer-predicate
   (lambda (buf) (not (cl-find-if (lambda (pattern) (string-match pattern (buffer-name buf))) helm-boring-buffer-regexp-list))))
  )

(use-package helm-ag
  :ensure t
  :after (helm)
  :commands helm-ag)

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config (helm-projectile-on))

(use-package magit
  :commands magit
  :ensure t)

(use-package company
  :ensure t)

(use-package flycheck
  :commands flycheck-mode
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package paredit
  :ensure t
  :commands paredit
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)))

(use-package faust-mode
  :ensure t
  :commands faust-mode
  :mode "\\.dsp\\'")

(use-package clojure-mode
  :ensure t
  :commands clojure-mode
  :mode "\\.clj\\'")

(use-package cider
  :ensure t
  :commands cider
  :after (clojure-mode)
  :hook clojure-mode)

(use-package swift-mode
  :ensure t
  :commands swift-mode
  :mode "\\.swift\\'")

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init (setq python-indent-offset 2))

(use-package elpy
  :ensure t
  :commands elpy
  :mode "\\.py\\'"
  :after (python-mode)
  :init (elpy-enable))

(use-package js-mode
  :mode ("\\.js\\'" "\\.jsx\\'")
  :init (setq js-indent-level tab-width))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'")
  :init (setq typescript-indent-level tab-width))

(use-package json-mode
  :mode ("\\.json\\'")
  :init
  (setq json-reformat:indent-width tab-width))

(use-package indium
  ;; Note: indium npm package must be installed
  :ensure f
  :commands (indium indium-launch indium-connect)
  :after (js-mode))

(use-package indium-helm-config
  :no-require t
  :after (helm indium)
  :config
  (add-many! helm-boring-buffer-regexp-list
             "indium-connection"
             "indium-debug-log"
             "json-process-client"
             "*node process*"))

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'"))

(use-package flycheck-rust
  :ensure t
  :after (flycheck rust-mode)
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package web-mode
  :ensure t
  :mode ("\\.html\\'"))

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'"))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'"))

(straight-use-package
 '(hoon-mode :type git :host github :repo "urbit/hoon-mode.el"))

(use-package hoon-mode
  :ensure t
  :straight t
  :mode ("\\.hoon\\'"))

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol\\'"))

(use-package solidity-flycheck
  :ensure t
  :after (cl flycheck solidity-mode)
  :init
  (setq solidity-comment-style 'slash)
  (setq solidity-solc-path "/usr/local/bin/solc")
  (setq solidity-flycheck-solium-checker-active nil)
  (setq solidity-flycheck-solc-checker-active t)
  (setq flycheck-solidity-solc-addstd-contracts t))

(use-package company-solidity
  :ensure t
  :after (company solidity-mode)
  :hook (solidity-mode . company-mode))
