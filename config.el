;; [[file:config.org::*configuration reload helper][configuration reload helper:1]]
(defun reload-config ()
  (interactive)
  (load-file user-init-file)
  (princ "Configuration reloaded."))

(global-set-key (kbd "C-x C-l") 'reload-config)
;; configuration reload helper:1 ends here

;; [[file:config.org::*specpdl and eval depth size][specpdl and eval depth size:1]]
(setq max-specpdl-size 500
      max-lisp-eval-depth 1000)
;; specpdl and eval depth size:1 ends here

;; [[file:config.org::*add some extra paths][add some extra paths:1]]
(setq exec-path (append exec-path '("/usr/local/bin" "/usr/bin")))
;; add some extra paths:1 ends here

;; [[file:config.org::*key bindings for OSX][key bindings for OSX:1]]
(when (eq system-type 'darwin)
  (setq
   frame-resize-pixelwise t
   menu-bar-mode t
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-control-modifier 'control))
;; key bindings for OSX:1 ends here

;; [[file:config.org::*repos][repos:1]]
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
;; repos:1 ends here

;; [[file:config.org::*straight][straight:1]]
(setq straight-repository-branch "develop"
      straight-use-package-by-default t)
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
;; straight:1 ends here

;; [[file:config.org::*use-package][use-package:1]]
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; use-package:1 ends here

;; [[file:config.org::*GUI tweaks][GUI tweaks:1]]
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(if (eq system-type 'darwin)
  (set-frame-font "Fantasque Sans Mono-14" nil t)
  (set-frame-font "Fantasque Sans Mono-12" nil t))
(global-hl-line-mode 1)
;; GUI tweaks:1 ends here

;; [[file:config.org::*theme][theme:1]]
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  (custom-theme-set-faces
   'modus-vivendi
   '(org-level-1 ((t (:weight semi-bold :height 2.0))))
   '(org-level-2 ((t (:weight semi-bold :height 1.5))))
   '(org-level-3 ((t (:weight semi-bold :height 1.25))))
   '(org-level-4 ((t (:weight semi-bold :height 1.0)))))
)
;; theme:1 ends here

;; [[file:config.org::*title line][title line:1]]
(setq frame-title-format
   (list (format "%s %%S: %%j " (system-name))
     '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; title line:1 ends here

;; [[file:config.org::*env][env:1]]
;; set the below so tmux knows not to load when we eval .zshrc
;; this is respected by ohmyzsh's tmux plugin
(setenv "EMACS" "1")
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
     (exec-path-from-shell-initialize)
;; extra environment variables to bring in, in addition to the standard ones like PATH
     (exec-path-from-shell-copy-env "GOPATH")
     (exec-path-from-shell-copy-env "GOBIN")
     (exec-path-from-shell-copy-env "GOFLAGS")))
;; env:1 ends here

;; [[file:config.org::*startup][startup:1]]
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)

(if (equal command-line-args '("emacs"))
    (setq initial-buffer-choice "~/Org/scratch.org")
)
;; startup:1 ends here

;; [[file:config.org::*global keybindings][global keybindings:1]]
;; this was a really fucking annoying, I finally worked out this shortcut was how I was locking up emacs.
(global-unset-key (kbd "C-z"))
;; global keybindings:1 ends here

;; [[file:config.org::*disable backup files][disable backup files:1]]
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
;; disable backup files:1 ends here

;; [[file:config.org::*which key?][which key?:1]]
(use-package which-key
  :ensure t)
;; which key?:1 ends here

;; [[file:config.org::*vterm][vterm:1]]
(use-package vterm
  :ensure t)
;; vterm:1 ends here

;; [[file:config.org::*completion][completion:1]]
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy)

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))
;; completion:1 ends here

;; [[file:config.org::*whitespace][whitespace:1]]
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))
;; whitespace:1 ends here

;; [[file:config.org::*major modes][major modes:1]]
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.ledger\\'" . hledger-mode))
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
(add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode))
;; major modes:1 ends here

;; [[file:config.org::*use-package][use-package:1]]
(use-package org
	   :config
	 )
;; use-package:1 ends here

;; [[file:config.org::*org directories][org directories:1]]
(setq org-directory
      (cond
       ((eq system-type 'darwin)
	"~/Library/Mobile Documents/com~apple~CloudDocs/Org/")
       ((eq system-type 'gnu/linux)
	"~/Org")))  
(setq org-agenda-files
      (cond
       ((eq system-type 'darwin)
	"~/Library/Mobile Documents/com~apple~CloudDocs/Org/")
       ((eq system-type 'gnu/linux)
	"~/Org")))  
(setq org-default-notes-file
      (cond
       ((eq system-type 'darwin)
	"~/Library/Mobile Documents/com~apple~CloudDocs/Org/TODO.org")
       ((eq system-type 'gnu/linux)
	"~/Org/TODO.org")))
;; org directories:1 ends here

;; [[file:config.org::*org shortcut functions][org shortcut functions:1]]
(defun org-daily ()
  (interactive)
  (let ((daily-name (format-time-string "%Y-%m-%d")))
    (find-file (expand-file-name (concat org-directory "/Scratch/" daily-name ".org")))))  
(defun todo ()
  (interactive)
  (find-file (expand-file-name (concat org-directory "/TODO.org"))))
;; org shortcut functions:1 ends here

;; [[file:config.org::*babel configuration][babel configuration:1]]
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (emacs-lisp . t)
   (awk . t)
   ))

(setq python-shell-completion-native-enable nil)
(setq org-latex-minted-options '(("breaklines" "true")
				 ("breakanywhere" "true")))
;; babel configuration:1 ends here

;; [[file:config.org::*extra TODO config][extra TODO config:1]]
(setq org-todo-keywords
      (quote ((sequence "TODO(t)"
			"NEXT(n!)"
			"WAIT(w!)"
			"DOING(i!)"
			"|"
			"DONE(d!)"
			))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "blue" :weight bold)
	      ("WAIT" :foreground "orange" :weight bold)
	      ("DOING" :foreground "orange" :weight bold)
	      ("DONE" :foreground "forest green" :weight bold)
	      )))

(setq org-use-fast-todo-selection t)
;; extra TODO config:1 ends here

;; [[file:config.org::*org TODO dependencies][org TODO dependencies:1]]
(setq org-enforce-todo-dependencies t)
;; org TODO dependencies:1 ends here

;; [[file:config.org::*org agenda][org agenda:1]]
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-window-setup "current-window")
;; org agenda:1 ends here

;; [[file:config.org::*org look n' feel][org look n' feel:1]]
(setq org-ellipsis " ►"
      org-hide-leading-stars t)
;; org look n' feel:1 ends here

;; [[file:config.org::*git][git:1]]
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))
;; git:1 ends here

;; [[file:config.org::*projectile][projectile:1]]
(use-package projectile
  :ensure t)
;; projectile:1 ends here

;; [[file:config.org::*company][company:1]]
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
    ("C-n". company-select-next)
    ("C-p". company-select-previous)
    ("M-<". company-select-first)
    ("M->". company-select-last))
  (:map company-mode-map
    ("<tab>". tab-indent-or-complete)
    ("TAB". tab-indent-or-complete)))
  :config
  (setq
     company-minimum-prefix-length 1
     company-idle-delay 0.0
     company-tooltip-align-annotations t)

  (defun company-yasnippet-or-completion ()
   (interactive)
   (or (do-yas-expand)
     (company-complete-common)))

  (defun check-expansion ()
    (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
    (if (looking-at "\\.") t
      (backward-char 1)
    (if (looking-at "::") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
    (if (or (not yas/minor-mode)
        (null (do-yas-expand)))
    (if (check-expansion)
        (company-complete-common)
      (indent-for-tab-command)))))
;; company:1 ends here

;; [[file:config.org::*flycheck][flycheck:1]]
(use-package flycheck
:ensure t)
;; flycheck:1 ends here

;; [[file:config.org::*snippets][snippets:1]]
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
;; snippets:1 ends here

;; [[file:config.org::*lsp mode][lsp mode:1]]
(use-package lsp-mode
  :ensure
  :commands lsp
  :config
  (setq lsp-keymap-prefix "C-c l"
    lsp-modeline-diagnostics-enable t
    lsp-file-watch-threshold nil
    lsp-enable-file-watchers t
    lsp-print-performance nil
    lsp-log-io nil
    lsp-idle-delay 0.6
    lsp-eldoc-render-all t
    company-minimum-prefix-length 1
    company-idle-delay 0.0
    company-tooltip-align-annotations t
    lsp-rust-analyzer-cargo-watch-command "clippy"
    lsp-rust-analyzer-server-display-inlay-hints t
    lsp-rust-analyzer-server-command '("~/.cargo/bin/rustup run nightly rust-analyzer")
    lsp-rust-analyzer-proc-macro-enable t
    lsp-clangd-binary-path "/System/Volumes/Data/Library/Developer/CommandLineTools/usr/bin/clangd"
    lsp-go-gopls-server-path "~/.go/bin/gopls"
    lsp-pylsp-server-command "~/.pyenv/versions/emacs39/bin/pylsp")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (defun lsp-save-hooks ()
     (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

(use-package lsp-ui
   :ensure
   :commands lsp-ui-mode
   :config
   (setq lsp-ui-peek-always-show t
    lsp-ui-sideline-show-hover nil
    lsp-ui-doc-enable nil))
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; lsp mode:1 ends here

;; [[file:config.org::*rust][rust:1]]
(use-package rustic
  :ensure
  :bind
    (:map rustic-mode-map
      ("M-j" . lsp-ui-imenu)
      ("M-?" . lsp-find-references)
      ("C-c C-c l" . flycheck-list-errors)
      ("C-c C-c a" . lsp-execute-code-action)
      ("C-c C-c r" . lsp-rename)
      ("C-c C-c q" . lsp-workspace-restart)
      ("C-c C-c Q" . lsp-workspace-shutdown)
      ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save nil)
  :hook ((rustic-mode . rk/rustic-mode-hook)))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))
;; rust:1 ends here

;; [[file:config.org::*clang / c][clang / c:1]]
(use-package lsp-mode
  :ensure t
  :hook (c-mode . lsp-deferred))
;; clang / c:1 ends here

;; [[file:config.org::*golang][golang:1]]
(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)))
;; golang:1 ends here

;; [[file:config.org::*python][python:1]]
(use-package python-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)))
;; python:1 ends here

;; [[file:config.org::*yaml][yaml:1]]
(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook
      (lambda ()
	      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
;; yaml:1 ends here

;; [[file:config.org::*mpdel][mpdel:1]]
(use-package mpdel
  :ensure t)

(use-package ivy-mpdel
  :ensure t)
;; mpdel:1 ends here
