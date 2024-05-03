;;; -*- lexical-binding: t -*-

;; Declare directory location variables
(defvar my-local-dir (expand-file-name "local/" user-emacs-directory)
  "The local directory contains miscellaneous files created by Emacs and my config.")
(defvar my-cache-dir (expand-file-name "cache/" user-emacs-directory)
  "The cache directory contains temporary data created by Emacs.")
(defvar my-lib-dir (expand-file-name "lib/" user-emacs-directory)
  "The lib folder contains personal elisp libraries I've written for my use.")

;; Bootstrap package manager
(setq straight-base-dir my-local-dir
      straight-use-package-by-default t
      straight-vc-git-default-clone-depth '(1 single-branch)
      straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Add personal library folder to the load path
(add-to-list 'load-path my-lib-dir)

;; Load libray files
(require 'my-misc)

;; Load the custom.el file
(setq custom-file (concat my-local-dir "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

;;; Graphical interface preferences

;; These are disbaled in the early-init, but these variables need to
;; be set if the toggle functions want to be used later.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Disbale tooltips
(tooltip-mode -1)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; in a daemon session where it'll say "Starting Emacs daemon." instead
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Change frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Disable the bell
(setq ring-bell-function #'ignore
      visible-bell nil)

;; From Doom Emacs
;; Set conding system to utf-8 by default
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(with-system windows-nt
  (setq selection-coding-system 'utf-8))

;; Set default font
(defvar my-default-font "DejaVuSansM Nerd Font-11")

(when (font-exists-p my-default-font)
  (set-frame-font my-default-font nil t))

;; Icons
(use-package nerd-icons
  :defer t)

;;; QOL improvements

;; Nice startup screen
(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Welcome Home"
        dashboard-set-footer nil
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        ;; dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-startup-banner (expand-file-name "fancy-emacs-logo.svg"
                                                   user-emacs-directory)
        dashboard-image-banner-max-height 400
        dashboard-image-banner-max-height 400
        dashboard-projects-backend 'project-el
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
    (setq dashboard-heading-icons '((recents . "nf-oct-history")
                                    (projects . "nf-oct-rocket")
                                    (agenda . "nf-oct-calendar")))
    :config
    (dashboard-setup-startup-hook))

;; Quiet startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Make yes/no prompts y/n
(setq use-short-answers t)

;; Automatically select help windows
(setq help-window-select t)

;; Tweak how auto-saving works
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat my-cache-dir "autosave/")
      tramp-auto-save-directory  (concat my-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Configure backups and lockfiles
(setq create-lockfiles nil
      remote-file-name-inhibit-locks t
      make-backup-files t
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 3
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat my-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;;; Basic text formatting

;; Sentences end in one space these days
(setq sentence-end-double-space nil)

;; Use spaces instead of tabs and set indention level
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Change the behavior of the tab key
(setq-default tab-always-indent t)

;; Continuation and truncation
(setq-default word-wrap t      ; wrap words at whitespace
              truncate-lines t ; disable wrapping and use truncation
              truncate-partial-width-windows nil)

;;; Optimzations

;; Increase amount of data Emacs reads from processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Slow down emacs ui updates
(setq idle-update-delay 1.0)

;; Font compacting is expensive
(setq inhibit-compacting-font-caches t)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
(setq frame-inhibit-implied-resize t)

;; Inhibits fontification while receiving input, which should help a little
;; with scrolling performance
(setq redisplay-skip-fontification-on-input t)

;;; Themes

(use-package zenburn-theme
  :defer t)

(use-package doom-themes
  :defer t
  :config
  (doom-themes-org-config))

;; Default themes
(defvar my-default-theme 'doom-gruvbox)
(defvar my-backup-theme 'leuven)

;; Load the default theme. If that fails, load the backup and alert
;; the user.
(unless (load-theme my-default-theme t)
  (message "Could not load theme, %s. Loading backup theme, %s."
           (symbol-value my-default-theme) (symbol-value my-backup-theme))
  (load-theme my-backup-theme t))

;;; Package/Features/Modes configuration

;; Garbage collection tweaks
(use-package gcmh
  :hook
  (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)))

;; Ibuffer
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

;; Tramp
(setq tramp-persistency-file-name (expand-file-name my-cache-dir
                                                    "tramp")
      ;; Use the user control master settings
      tramp-use-ssh-controlmaster-options nil)

;; Init profiler
(use-package esup
  :defer t
  :config
  (setq esup-depth 0))

;; Whichkey
(use-package which-key
  :hook
  (after-init . which-key-mode))

;; Recent files
(use-package recentf
  :hook
  (after-init . recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-cache-dir)
        recentf-max-saved-items 50))

;; Desktop mode
(use-package desktop
  :defer t
  :config
  (setq desktop-path (list (concat my-local-dir "desktop/"))))

(use-package bookmark
  :defer t
  :init
  (setq bookmark-default-file (concat my-local-dir "bookmarks")))

;; show-paren-mode
(use-package paren
  :defer t
  :config
  (setq show-paren-context-when-offscreen 'overlay))

;; Autofill
(add-hook 'prog-mode-hook #'auto-fill-mode)

;; Fill column
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Highlight line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Electric pairs
(use-package elec-pair
  :hook
  (prog-mode . electric-pair-mode))

;; Whitespace
(use-package whitespace
  :defer t)

(use-package ws-butler
  :hook
  ((prog-mode . ws-butler-mode)))

;; Saveplace
(use-package saveplace
  :hook
  (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name "saveplace" my-local-dir)))

;; Transient mark mode
;;;###autoload
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;;;###autoload
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(global-set-key (kbd "C-`") #'push-mark-no-activate)
(global-set-key (kbd "M-`") #'jump-to-mark)

;; Line numbers
(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode))
  :bind
  ("C-c m l" ("line numbers" . display-line-numbers-mode)))

;; Rainbow parens
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Hydra
(use-package hydra
  :bind
  ("C-x R" . hydra-resize/body)
  :config
  (defhydra hydra-resize ()
    "resize"
    ("h" enlarge-window-horizontally)
    ("l" shrink-window-horizontally)
    ("j" enlarge-window)
    ("k" shrink-window)
    ("q" nil "quit")))

;; Helm
(use-package helm
  :hook
  (after-init . helm-mode)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("C-'" . helm-mark-ring)
   ("C-s" . helm-occur)
   ("C-h a" . helm-apropos)
   ("M-y" . helm-show-kill-ring)
   ([remap imenu] . helm-imenu))
  :config
  (setq helm-apropos-fuzzy-match t)

  (use-package helm-xref))

;; Company
(use-package company
  :defer t
  :bind
  (:map company-active-map
   ("<tab>" . company-complete-common-or-cycle))
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
        company-show-quick-access t))

;; Modeline
(use-package doom-modeline
  :defer t
  :hook
  (after-init . (lambda ()
                  (doom-modeline-mode 1)
                  (column-number-mode 1)))
  :config
  (with-system windows-nt
    (setq doom-modeline-icon nil)))

;; Treesitter
(when (and (>= emacs-major-version 29)
           (eq system-type 'gnu/linux))
  (use-package tree-sitter
    :defer t
    :hook
    ((c++-mode . (lambda ()
                   (tree-sitter-mode 1)
                   (tree-sitter-hl-mode 1))))
    :config
    (use-package tree-sitter-langs)))

;; Avy
(use-package avy
  :bind
  (("M-g M-g" . avy-goto-line)
   ("C-;" . avy-goto-char-timer)))

;; Ace window and windows in general
(use-package ace-window
  :bind
  ("M-o" . ace-window))

;; (my-window-def
;;   "v" 'split-window-right
;;   "h" 'split-window-below)

;; Projects
(use-package project
  :defer t
  :config
  (setq project-list-file (expand-file-name "projects" my-local-dir)))

;; Snippets
(use-package yasnippet
  :defer t)

;; Lsp client
(use-package lsp-mode
  :defer t
  :bind-keymap
  (("C-," . lsp-command-map))
  :hook
  ((c++-mode . (lambda ()
                 (yas-minor-mode 1)
                 (lsp-deferred)
                 (lsp-enable-which-key-integration))))
  :init
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-enable-suggest-server-download nil
        lsp-enable-text-document-color nil
        lsp-enable-folding nil
        lsp-lens-enable t
        lsp-inlay-hint-enable t
        lsp-eldoc-enable-hover t
        lsp-session-file (expand-file-name "lsp-session"
                                           my-cache-dir)
        lsp-keymap-prefix "C-,")
  :config
  (use-package lsp-ui
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-diagnostics t))

  (use-package helm-lsp
    :bind
    (:map lsp-mode-map
          ([remap xref-find-apropos] . helm-lsp-workspace-symbol))))

(use-package eglot
  :disabled t
  :hook
  ((c++-mode . (lambda ()
                 (eglot-ensure)
                 (company-mode 1)
                 (yas-minor-mode 1)))
   (rust-mode . (lambda ()
                  (eglot-ensure)
                  (company-mode 1)
                  (yas-minor-mode 1)))
   (cmake-mode . (lambda ()
                   (eglot-ensure)
                   (company-mode 1)
                   (yas-minor-mode 1))))
  :config
  (setq eglot-autoshutdown t))

;; Hungry delete
(use-package smart-hungry-delete
  :bind
  (([remap delete-backward-char] . smart-hungry-delete-backward-char)
   ([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
   ([remap delete-char] . smart-hungry-delete-forward-char))
  :init
  (smart-hungry-delete-add-default-hooks))

;; Transient
(use-package transient
  :defer t
  :config
  (setq transient-history-file (expand-file-name "transient-history" my-local-dir)))

;; Git client
(use-package magit
  :defer t
  :config
  (with-system windows-nt
    (setenv "SSH_ASKPASS" "git-gui--askpass")))

;; Eshell
(use-package eshell
  :defer t
  :config
  (defun eshell-other-window (use-this-window-p)
    "Open a new eshell buffer in other window or use the same window."
    (interactive "P")
    (let ((buff (eshell)))
      (if use-this-window-p
          (switch-to-buffer buff)
        (switch-to-buffer (other-buffer buff))
        (switch-to-buffer-other-window buff))))

  (use-package eshell-prompt-extras
    :config
    (setq eshell-prompt-function 'epe-theme-lambda))

  (setq eshell-directory-name (expand-file-name "eshell"
                                                my-local-dir)))

(use-package eat
  :defer t
  :config
  (defun eat-other-window (use-this-window-p)
    "Open a new eat buffer in other window or use the same window."
    (interactive "P")
    (let ((buff (eat)))
      (if use-this-window-p
          (switch-to-buffer buff)
        (switch-to-buffer (other-buffer buff))
        (switch-to-buffer-other-window buff)))))

;; Treemacs
(use-package treemacs
  :defer t)

;; Server settings
(use-package server
  :defer t
  :config
  (setq server-auth-dir (expand-file-name "server" my-local-dir)))

;;; Programming languages

;; Programming modes
(define-key prog-mode-map (kbd "C-M-;") #'uncomment-region)

;; Org
(use-package org
  :defer t
  :bind
  (("C-c m a" . org-agenda)
   ("C-c m c" . org-capture))
  :config
  (setq org-startup-indented t
        org-src-fontify-natively t
        org-image-actual-width nil)
  ;; (setq org-enforce-todo-dependencies t
  ;;       org-log-done 'time
  ;;       org-agenda-todo-list-sublevels nil
  ;;       org-todo-keywords
  ;;       '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)"))
  ;;       org-todo-keyword-faces
  ;;       `(("STARTED" . ,(face-attribute 'ansi-color-blue
  ;;                                       :foreground))))

  ;; (add-to-list 'org-faces-easy-properties
  ;;              (cons (face-attribute 'ansi-color-blue :foreground) :foreground))
  (setq org-agenda-files (list "~/org/notes.org"
                               "~/org/school.org")
        org-default-notes-file (concat org-directory "/notes.org")
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        org-display-custom-times t
        org-time-stamp-custom-formats '("<%m/%d/%y %a>" . "<%m/%d/%y %a %I:%M %p>"))

  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n %u\n %a\n")
          ("T" "Test" entry (file+headline  "~/org/school.org" "Tests")
           "* TODO %?\n %t\n %^{CLASS}p\n")
          ("a" "Assignment" entry (file+headline "~/org/school.org" "Assignments")
           "* TODO %?\n DEADLINE: %t\n %^{CLASS}p\n")))

  (use-package helm-org
    :config
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-capture . helm-org-completing-read-tags))
    (add-to-list 'helm-completing-read-handlers-alist
                 '(org-set-tags . helm-org-completing-read-tags)))

  (use-package org-preview-html
    :config
    (setq org-preview-html-refresh-configuration 'save)))

;; Emacs Lisp
;; (my-major-def
;;   :keymaps 'emacs-lisp-mode-map
;;   "m" 'emacs-lisp-macroexpand
;;   "b" 'eval-buffer
;;   "r" 'eval-region)

(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (company-mode 1)))

;; C/C++
(use-package cc-mode
  :defer t
  :config
  (defconst my-cpp-style
    '("linux"
      (c-basic-offset . 4)
      (c-doc-comment-style . doxygen)
      (c-indent-comments-syntactically-p . t)
      (c-hanging-braces-alist . ())
      (c-hanging-colons-alist . ((access-label . (after))))
      (c-offsets-alist . ((inline-open . 0)
                          (innamespace . 0)))
      (c-cleanup-list . (brace-else-brace
                         brace-eleseif-brace
                         defun-close-semi
                         comment-close-slash))))

  (c-add-style "my-cpp-style" my-cpp-style)

  (setq c-default-style '((c++-mode . "my-cpp-style")
                          (java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))

  (setq-default c-auto-newline t))

(use-package nasm-mode)

(use-package disaster
  :init
  (setq disaster-assembly-mode 'nasm-mode))

;; Cmake
(use-package cmake-mode
  :defer t)

;; Yuck
(use-package yuck-mode
  :defer t)

;; Python
(use-package lsp-pyright
  :hook
  ((python-mode . lsp)))

;; Rust
(use-package rust-mode
  :defer t)

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (define-key cargo-mode-map (kbd "C-, C-c") 'cargo-minor-mode-command-map))

;; Common Lisp
(use-package sly
  :defer t
  :hook
  ((lisp-mode . (lambda ()
                  (company-mode 1)
                  (setq-local fill-column 100)
                  (sly-mode)))
   (sly-mrepl . (lambda ()
                  (company-mode 1))))
  :config
  (use-package sly-quicklisp)
  (use-package sly-asdf)
  (setq inferior-lisp-program "sbcl"
        sly-contribs '(sly-fancy sly-quicklisp sly-asdf)))
