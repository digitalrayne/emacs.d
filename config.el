;; [[file:config.org::*configuration reload helper][configuration reload helper:1]]
(defun reload-config ()
  (interactive)
  (load-file user-init-file)
  (princ "Configuration reloaded.")
  )

(global-set-key (kbd "C-x C-l") 'reload-config)
;; configuration reload helper:1 ends here

;; [[file:config.org::*specpdl and eval depth size][specpdl and eval depth size:1]]
(setq max-specpdl-size 13000
      max-lisp-eval-depth 100000)
;; specpdl and eval depth size:1 ends here

;; [[file:config.org::*key bindings for OSX][key bindings for OSX:1]]
(when (eq system-type 'darwin)
  (setq
   frame-resize-pixelwise t
   menu-bar-mode t
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-control-modifier 'control))
;; key bindings for OSX:1 ends here

;; [[file:config.org::*Repos][Repos:1]]
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
;; Repos:1 ends here

;; [[file:config.org::*Straight][Straight:1]]
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
;; Straight:1 ends here

;; [[file:config.org::*use-package][use-package:1]]
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; use-package:1 ends here

;; [[file:config.org::*GUI tweaks][GUI tweaks:1]]
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(set-frame-font "Fantasque Sans Mono 14" nil t)
;; GUI tweaks:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t)
  )
;; Theme:1 ends here

;; [[file:config.org::*Title line][Title line:1]]
(setq frame-title-format
   (list (format "%s %%S: %%j " (system-name))
     '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; Title line:1 ends here

;; [[file:config.org::*env][env:1]]
;; set the below so tmux knows not to load when we eval .zshrc
;; this is respected by ohmyzsh's tmux plugin
(setenv "EMACS" "1")
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
     (exec-path-from-shell-initialize)
;; extra environment variables to bring in, in addition to the standard ones like PATH
     (exec-path-from-shell-copy-env "GOPROXY")
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

;; [[file:config.org::*which key?][which key?:1]]
(use-package which-key
  :ensure t)
;; which key?:1 ends here

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
(setq org-directory '("~/Org"))
(setq org-agenda-files '("~/Org"))
(setq org-default-notes-file '("~/Org/TODO.org"))
;; org directories:1 ends here

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
  :ensure t
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-tooltip-align-annotations t))
;; company:1 ends here

;; [[file:config.org::*flycheck][flycheck:1]]
(use-package flycheck
:ensure t)
;; flycheck:1 ends here

;; [[file:config.org::*configure lsp mode][configure lsp mode:1]]
(use-package lsp-mode
  :config
  (setq lsp-keymap-prefix "C-c l"
    lsp-modeline-diagnostics-enable t
    lsp-file-watch-threshold nil
    lsp-enable-file-watchers t
    lsp-print-performance nil
    lsp-log-io nil
    lsp-idle-delay 0.500
    company-minimum-prefix-length 1
    company-idle-delay 0.0
    company-tooltip-align-annotations t
    lsp-rust-all-features t      
    lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")
    lsp-clangd-binary-path "/System/Volumes/Data/Library/Developer/CommandLineTools/usr/bin/clangd"
    lsp-rust-rls-server-command '("~/.cargo/bin/rls")
    lsp-go-gopls-server-path "~/.go/bin/gopls"
    lsp-pylsp-server-command "~/.pyenv/versions/emacs39/bin/pylsp")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (defun lsp-save-hooks ()
    ((add-hook 'before-save-hook #'lsp-format-buffer t t)
     (add-hook 'before-save-hook #'lsp-organize-imports t t)))

  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list))
;; configure lsp mode:1 ends here

;; [[file:config.org::*rust][rust:1]]
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :bind
  ("C-c g" . rust-run)
  ("C-c t" . rust-test)
  ("C-c b" . cargo-process-build)
  :init
  (which-function-mode 1)
  (setq compilation-error-regexp-alist-alist
      (cons '(cargo "^\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([0-9]+\\):\\([0-9]+\\) \\(?:[Ee]rror\\|\\([Ww]arning\\)\\):" 1 (2 . 4) (3 . 5) (6))
	compilation-error-regexp-alist-alist)))
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
