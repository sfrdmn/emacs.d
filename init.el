(let ((minver "24"))
  (when (version<= emacs-version minver)
    (error "This init.el requires emacs v%s or higher" minver)))

;;;; el-get
;; Used to require packages from places other than ELPA MELPA whatever

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer (url-retrieve-synchronously
                        "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get-bundle el-get)
(el-get-bundle typo)
(el-get-bundle arcadia-unity/arcadia-dot-el)

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
(el-get 'sync)

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
;; .dir-locals.el helpers
(defun project-path (&optional relative-path)
  (expand-file-name
   (concat
    (locate-dominating-file (or buffer-file-name default-directory) ".dir-locals.el")
    (or relative-path ""))))
;; Pos-Tip
(require-package 'pos-tip)
;; Rainbow delim
(require-package 'rainbow-delimiters)
(global-prettify-symbols-mode 1)

;;;; GUI

(defun set-emoji-font (frame)
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(if (display-graphic-p)
    (progn
      ;;;; Solarized
      (require-package 'solarized-theme)
      (load-theme 'solarized-dark)
      (setq solarized-high-contrast-mode-line t)

      ;;;; Exec path config
      (require-package 'exec-path-from-shell)
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize))

      ;;;; Emoji
      ;; (require-package 'company-emoji)
      ;; (add-hook 'company-mode-hook (lambda ()
      ;;                                (add-to-list 'company-backends 'company-emoji)))
      ;; (set-emoji-font nil)
      ;; (add-hook 'after-make-frame-functions 'set-emoji-font)
      (require-package 'emojify)
      (add-hook 'after-init-hook #'global-emojify-mode)

      ;;;; Misc
      (scroll-bar-mode -1)
      (setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-follow-mouse 't)
      (require-package 'eww)
      (setq helm-dash-browser-func 'eww-browse-url)))

;;;; Electric pair

(add-hook 'prog-mode-hook 'electric-pair-mode)
(defun electric-pair-sq ()
  (setq-local electric-pair-pairs
              (append electric-pair-pairs '((?\' . ?\')))))
(add-hook 'javascript-mode-hook 'electric-pair-sq)
(add-hook 'js2-mode-hook 'electric-pair-sq)
(add-hook 'web-mode-hook 'electric-pair-sq)

;;;; Grep

(require 'grep)
(setq grep-find-ignored-files
      (append grep-find-ignored-files
              '(".DS_Store" ".flowconfig" ".nvmrc" ".istanbul.yml"
                ".projectile" ".bablrc" ".buckconfig" ".dir-locals.el"
                ".eslintrc" ".gitignore" ".travis.yml" ".watchmanconfig"
                "#*#")))
(setq grep-find-ignored-directories
      (append grep-find-ignored-directories
              '("node_modules")))
(require 'grep)

;;; MMM Mode

(require-package 'mmm-mode)
(require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 1)

;;;; Dired+

(require-package 'dired+)
;; Reuse single dired buffer
(diredp-toggle-find-file-reuse-dir 1)

;;;; Magit

(require-package 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;;;; Git Timemachine

(require-package 'git-timemachine)

;;;; Org Mode

(defvar org-fontify-whole-heading-line t)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

;;;; Markdown

(require-package 'markdown-mode)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("Readme\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook (lambda () (setq fill-column 80)))
(setq markdown-command "remark")

(mmm-add-classes
 '((markdown-javascript
    :submode js-mode
    :front "^```[Jj]ava[Ss]cript[\s\n\r]+"
    :back "^```\s*$")

   (markdown-jsx
    :submode web-mode
    :front "^```[Jj][Ss][Xx][\s\n\r]+"
    :back "^```\s*$")

   (markdown-clojure
    :submode clojure-mode
    :front "^```[Cc]lojure[\s\n\r]+"
    :back "^```\s*$")))

(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-javascript)
(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-clojure)
(setq mmm-parse-when-idle 't)

;;;; Spellcheck

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; Text-ish modes

(setq-default typo-language 'English)
(add-hook 'text-mode-hook
          (lambda ()
            (if (not (derived-mode-p 'html-mode 'yaml-mode))
                (typo-mode))))
(global-set-key (kbd "C-c C-t") 'typo-mode)

;;;; Helm

(require-package 'helm)
(helm-mode 1)
(require-package 'helm-unicode)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-:") 'helm-eval-expression)
;; Allows using <tab> for selecting highlighted completion options
;; (normally <tab> would open a context menu)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(setq helm-split-window-in-side-p t)

;;;; Projectile

(require-package 'projectile)
(projectile-mode)
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
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)

;;;; Helm-Dash

(require-package 'helm-dash)

;;;; Company Mode

(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook (lambda ()
                               (add-to-list 'company-dabbrev-code-modes 'web-mode)))

;;;; Flycheck

(require-package 'flycheck)
(require 'flycheck)
(require-package 'flycheck-tip)
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
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;;; Clojure

(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'helm-cider)
(helm-cider-mode 1)
;; Crazy symbolic programming
(setq
 clojure--prettify-symbols-alist
 (append '(("alpha" . ?α)
           ("beta" . ?β)
           ("gamma" . ?γ)
           ("delta" . ?δ)
           ("phi" . ?φ)
           ("-phi" . (?- (Br . Bl) ?φ))
           ("pi" . ?π)
           ("lambda" . ?λ)
           ("#(" . (?λ (Br . Bl) 40))           
           ("fn" . ?λ))))

;; Arcadia
(require 'arcadia)
(setq projectile-globally-unignored-files
      (append '("configuration.edn")
              projectile-globally-unignored-files))

;;;; Speedbar

(require-package 'sr-speedbar)

;;;; C / C++

(require-package 'cc-mode)
(require-package 'company-c-headers)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-c-headers))
(with-eval-after-load 'company-c-headers
  (add-to-list 'company-c-headers-path-system "/usr/local/opt/icu4c/include/"))

;;;; Rust

(require-package 'rust-mode)
(require-package 'cargo)
(require-package 'flycheck-rust)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook (lambda () (setq tab-width 0)))

;;;; Haskell

(require-package 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

(require-package 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;;; Web stuff
;; web-mode for all kinda templating languages
(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-enable-css-colorization t)
(setq web-mode-attr-indent-offset 2)

(add-hook 'web-mode-hook 'electric-indent-local-mode)
(setq web-mode-indentation-params
      '(("lineup-args" . t)
        ("lineup-calls" . nil)
        ("lineup-concats" . t)
        ("lineup-quotes" . t)
        ("lineup-ternary" . t)
        ("case-extra-offset" . t)))
;; Hide/Show
;; (add-to-list hs-special-modes-alist '(web-mode "{" "}" "/[*/]" nil))
;; Make syntax highlighting less shitty
(add-hook 'web-mode-hook
          (lambda ()
            (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
            ;; Reload local variables... because why?
            ;; (hack-local-variables)
            ))
;; Use single line comments for regions

;;;; Node / JS

(defvaralias 'js-indent-level 'tab-width)
(require-package 'nvm)
(load "nvm") ;; Why do I have to do this?
(require-package 'js-comint)
(require-package 'js2-mode)
(setq js2-strict-missing-semi-warning nil)
(setq js2-bounce-indent-p t)
(setq js2-include-node-externs t)
(add-hook 'javascript-mode-hook 'electric-indent-local-mode)
(add-hook 'js2-mode-hook 'electric-indent-local-mode)
(require-package 'prettier-js)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defun set-node-version (version)
  (interactive "sNode version: ")
  (nvm-use version))

;;;; Erlang

(defun with-erlang-config (init)
"
Implements custom Erlang config via reading an erlang.el file
Example: ((root-dir . \"path to erlang dir\")
          (tools-version . \"version of erlang tools package containing emacs mode\")
"
  (let ((erlang-config (f-relative "erlang.el" default-directory)))
    (when (file-exists-p erlang-config)
      (with-temp-buffer
        (insert-file-contents erlang-config)
        (goto-char (point-min))
        (let* ((config (read (current-buffer)))
                  (root-dir (alist-get 'root-dir config))
                  (tools-ver (alist-get 'tools-version config))
                  (emacs-dir (format "%s/lib/tools-%s/emacs" root-dir tools-ver)))
          (funcall init root-dir emacs-dir))))))

(defun flycheck-define-erlang-checker ()
  (flycheck-define-checker erlang-otp
    "Syntax checker for Erlang"
      :command
      ("erlc" "-o" temporary-directory "-Wall"
       "-I" "../include"
       "-I" "../../include"
       "-I" "../../../include"
       source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start (file-name) ":" line ": " (message) line-end))
      :modes erlang-mode))

(with-erlang-config
 (lambda (root-dir emacs-dir)
   ;; General
   (setq load-path (cons emacs-dir load-path))
   (require 'erlang-start)
   (setq erlang-root-dir root-dir)
   (setq erlang-man-root-dir (format "%s/man" root-dir))
   (setq exec-path (cons (format "%s/bin" root-dir) exec-path))

   ;; Distel
   ; so apparently distel needs LaTEX to compile... thanks erlang
   ;; (when (eq system-type 'darwin)
   ;;   (setq exec-path (cons "/Library/TeX/texbin" exec-path)))
   (el-get 'sync '(distel))
   (distel-setup)

   ;; Company mode Distel
   (require-package 'company-distel)
   (with-eval-after-load 'company
     (add-to-list 'company-backends 'company-distel))

   ;; Flycheck
   (flycheck-define-erlang-checker)
   (add-hook 'erlang-mode-hook (lambda () (flycheck-select-checker 'erlang-otp)))))

;;;; YAML

(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;; Docker

(require-package 'docker)
(require-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;;; EditorConfig

(require-package 'editorconfig)
(editorconfig-mode 1)

;;;; NeoTree

(require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;; YasSnippets

(require-package 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs "~/.emacs.d/snippets")

;;;; EMMS

(require-package 'emms)
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(when (eq system-type 'darwin)
  (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay)))

;;;; Ruby

(require-package 'rvm)
(rvm-use-default)

;;;; Sublimity

;; (require-package 'sublimity)
;; (require 'sublimity-map)
;; (sublimity-mode 1)
;; (setq sublimity-map-size 20)
;; (setq sublimity-map-fraction 0.3)
;; (setq sublimity-map-text-scale -7)

;;;; Elm

(require-package 'elm-mode)
(require-package 'flycheck-elm)
 (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;;;; Scheme

(require-package 'geiser)

;;;; C#

(require-package 'omnisharp)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;;; Poker!

(require-package 'poker)
(setq poker-suits '(♥ ♦ ♠ ♣))

;;;; Folding

(require-package 'origami)

;;;; Color code display

(require-package 'rainbow-mode)

;;;; Auto-complete

(require-package 'auto-complete)
(ac-config-default)

;;;; Indium

(require-package 'indium)
(add-hook 'js2-mode-hook #'indium-interaction-mode)

;;;; OSX

(when (eq system-type 'darwin)
  ;; Allow editing of binary .plist files.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-; commentnvert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  
  ;;It is necessary to perform an update!
  (jka-compr-update))
