;; [[file:config.org::*Configuration reload helper][Configuration reload helper:1]]
(defun reload-config ()
  (interactive)
  (org-babel-load-file
  (expand-file-name "config.org"
                 user-emacs-directory))
  (load-file user-init-file)
  (princ "Configuration reloaded."))

(global-set-key (kbd "C-x C-l") 'reload-config)
;; Configuration reload helper:1 ends here

;; [[file:config.org::*Tune specpdl and eval depth size][Tune specpdl and eval depth size:1]]
(setq max-specpdl-size 500
      max-lisp-eval-depth 1000)
;; Tune specpdl and eval depth size:1 ends here

;; [[file:config.org::*Set minimum native-comp warning level][Set minimum native-comp warning level:1]]
(setq warning-minimum-level :error)
;; Set minimum native-comp warning level:1 ends here

;; [[file:config.org::*Add some useful OS PATHs][Add some useful OS PATHs:1]]
(setq exec-path (append exec-path '("/usr/local/bin" "/opt/homebrew/bin" "/usr/bin")))
;; Add some useful OS PATHs:1 ends here

;; [[file:config.org::*Set key bindings for OSX][Set key bindings for OSX:1]]
(when (eq system-type 'darwin)
  (setq
   frame-resize-pixelwise t
   menu-bar-mode t
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-control-modifier 'control))
;; Set key bindings for OSX:1 ends here

;; [[file:config.org::*Repos][Repos:1]]
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
;; Repos:1 ends here

;; [[file:config.org::*Set up use-package][Set up use-package:1]]
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))
;; Set up use-package:1 ends here

;; [[file:config.org::*Icons][Icons:1]]
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)
;; Icons:1 ends here

;; [[file:config.org::*GUI tweaks][GUI tweaks:1]]
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)
(if (eq system-type 'darwin)
  (set-frame-font "Sarasa Mono J-14:Regular" nil t)
  (set-frame-font "Sarasa Mono J-12:Regular" nil t))
(global-hl-line-mode 1)
;; GUI tweaks:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
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
;; Theme:1 ends here

;; [[file:config.org::*Title line][Title line:1]]
(setq frame-title-format
   (list (format "%s %%S: %%j " (system-name))
     '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; Title line:1 ends here

;; [[file:config.org::*startup][startup:1]]
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
;; startup:1 ends here

;; [[file:config.org::*global keybindings][global keybindings:1]]
;; this was a really fucking annoying process, I finally worked out this shortcut was how I was locking up emacs.
(global-unset-key (kbd "C-z"))
;; global keybindings:1 ends here

;; [[file:config.org::*disable backup files][disable backup files:1]]
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
;; disable backup files:1 ends here

;; [[file:config.org::*which key?][which key?:1]]
(use-package which-key
  :config
  (which-key-mode))
;; which key?:1 ends here

;; [[file:config.org::*vterm][vterm:1]]
(use-package vterm
  :ensure t)
;; vterm:1 ends here

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
	"~/Org")
(setq org-agenda-files
	'("~/Org"))
(setq org-default-notes-file
	"~/Org/TODO.org")
;; org directories:1 ends here

;; [[file:config.org::*org inline images][org inline images:1]]
(setq org-startup-with-inline-images t)
;; org inline images:1 ends here

;; [[file:config.org::*org mermaid][org mermaid:1]]
(use-package ob-mermaid
  :config
  (org-babel-do-load-languages
  'org-babel-load-languages
  '((mermaid . t)
    (scheme . t)))
)
(add-hook 'org-babel-after-execute-hook
	  (lambda ()
	    (org-redisplay-inline-images)))
;; org mermaid:1 ends here

;; [[file:config.org::*git][git:1]]
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))
;; git:1 ends here

;; [[file:config.org::*projectile][projectile:1]]
(use-package projectile
  :ensure t
  :pin melpa-stable
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
;; projectile:1 ends here

;; [[file:config.org::*flymake][flymake:1]]
(use-package flymake
  :ensure nil
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
;; flymake:1 ends here

;; [[file:config.org::*rust mode][rust mode:1]]
(use-package rust-mode
  :ensure t)
;; rust mode:1 ends here

;; [[file:config.org::*LSP (lsp-mode)][LSP (lsp-mode):1]]
(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp)
	 (ruby-mode . lsp)
	 (rust-mode . lsp)
	 (python-mode . lsp)
	 (c-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind
  (("C-c C-c r" . lsp-restart-workspace))
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-modeline-diagnostics-enable t
	lsp-file-watch-threshold nil
	lsp-enable-file-watchers t
	lsp-print-performance nil
	lsp-idle-delay 0.6
	lsp-eldoc-render-all t
	lsp-rust-analyzer-server-display-inlay-hints t
	lsp-rust-analyzer-proc-macro-enable t
	lsp-rust-analyzer-server-command (list (replace-regexp-in-string "\n$" "" (shell-command-to-string "rustup which rust-analyzer")))
	lsp-clangd-binary-path "/System/Volumes/Data/Library/Developer/CommandLineTools/usr/bin/clangd"
	lsp-go-gopls-server-path "~/.go/bin/gopls"
	lsp-pylsp-server-command "~/.pyenv/versions/emacs39/bin/pylsp")
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("gopls.matcher" "CaseSensitive"))))

(use-package lsp-ui :commands lsp-ui-mode)
;; LSP (lsp-mode):1 ends here

;; [[file:config.org::*flycheck][flycheck:1]]
(use-package flycheck
  :ensure t
  :bind
  (("C-c C-c l" . flycheck-list-errors)
   ("C-c C-c n" . flycheck-next-error)
   ("C-c C-c p" . flycheck-previous-error)
   ("C-c C-c 1" . flycheck-first-error)))
;; flycheck:1 ends here

;; [[file:config.org::*company][company:1]]
(use-package company
  :ensure t
  :after eglot
  :hook (eglot-managed-mode . company-mode)
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-tooltip-align-annotations t
   tab-always-indent 'complete)

  (defun check-expansion ()
    (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
    (if (looking-at "\\.") t
      (backward-char 1)
    (if (looking-at "::") t nil))))))
;; company:1 ends here

;; [[file:config.org::*yaml][yaml:1]]
(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook
      (lambda ()
	      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))
;; yaml:1 ends here

;; [[file:config.org::*mastodon][mastodon:1]]
(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://"
    mastodon-active-user "ec0"))
;; mastodon:1 ends here

;; [[file:config.org::*matrix][matrix:1]]
(use-package ement
  :ensure t)
;; matrix:1 ends here

;; [[file:config.org::*mpdel][mpdel:1]]
(use-package mpdel
  :ensure t)
;; mpdel:1 ends here

;; [[file:config.org::*a nice dashboard][a nice dashboard:1]]
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
  (setq
   initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
   dashboard-set-footer nil
   dashboard-projects-backend 'projectile
   dashboard-set-init-info t
   dashboard-set-navigator t
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   dashboard-items '((recents  . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5)
		     (registers . 5)))
;; a nice dashboard:1 ends here
