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
      straight-use-package-by-default t)

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

;; Set conding system to utf-8 by default
(set-default-coding-systems 'utf-8)

;; Set default font
(defvar my-default-font "Dejavu Sans Mono-11")

(when (font-exists-p my-default-font)
  (set-frame-font "Dejavu Sans Mono-11" nil t))

;;; QOL improvements

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
(setq-default tab-always-indent 'complete)

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

;; Zenburn
(use-package zenburn-theme
  :defer t)

;; Default themes
(defvar my-default-theme 'zenburn)
(defvar my-backup-theme 'leuven)

;; Load the default theme. If that fails, load the backup and alert
;; the user.
(unless (load-theme my-default-theme t)
  (message "Could not load theme, %s. Loading backup theme, %s."
           (symbol-value my-default-theme) (symbol-value my-backup-theme))
  (load-theme my-backup-theme t))

;;; Package/Features/Modes configuration

;; General
(use-package general
  :defer t)

;; Whichkey
(use-package which-key
  :hook
  (after-init . which-key-mode))

;; Recent files
(use-package recentf
  :hook
  (after-init . recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-cache-dir)))

;; Autofill
(add-hook 'prog-mode-hook #'auto-fill-mode)

;; Electric pairs
(use-package elec-pair
  :hook
  (prog-mode . electric-pair-mode))

;; Whitespace
(use-package whitespace
  :defer t)

;; Saveplace
(use-package saveplace
  :hook
  (after-init . save-place-mode))

;; Transient mark mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(general-def
  "C-`" 'push-mark-no-activate
  "M-`" 'jump-to-mark)

;; Line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;; Rainbow pans
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Helm
(use-package helm
  :hook
  (after-init . helm-mode)
  :general
  ("M-x" 'helm-M-x)
  ("C-x C-f" 'helm-find-files)
  ("C-x b" 'helm-mini)
  ("C-'" 'helm-mark-ring)
  ("C-s" 'helm-occur)
  ("C-h a" 'helm-apropos)
  ("M-y" 'helm-show-kill-ring)
  :config
  (setq helm-apropos-fuzzy-match t))

;; Company
(use-package company
  :defer t
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; Avy
(use-package avy
  :general
  ("M-g M-g" 'avy-goto-line)
  ("C-;" 'avy-goto-char)
  ("C-:" 'avy-goto-char-2))

;; Snippets
(use-package yasnippet
  :defer t)

;; LSP Client
(use-package lsp-mode
  :hook
  ((lsp-mode . (lambda ()
                 (lsp-enable-which-key-integration)))
   (c++-mode . lsp))
  :config
  (setq lsp-enable-on-type-formatting nil
        lsp-idle-delay 0.1
        lsp-session-file (expand-file-name "lsp-session"
                                           my-cache-dir))
  
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;;; Programming languages

;; C/C++
(use-package cc-mode
  :hook
  ((c++-mode . (lambda ()
                 (yas-minor-mode 1))))
  :config
  (defconst my-cpp-style
    '("linux"
      (c-basic-offset . 4)
      (c-doc-comment-style . doxygen)
      (c-indent-comments-syntactically-p . t)
      (c-hanging-braces-alist . ())
      (c-offsets-alist . ((inline-open . 0)))
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

;; Yuck
(use-package yuck-mode
  :defer t)
