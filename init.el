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

;; General for keybinds
(use-package general
  :defer t
  ;; :commands
  ;; (my-create-prefix-definer)
  :config
  (defconst leader-prefix "C-c m")
  (defconst major-prefix "C-.")
  
;;   (defmacro my-create-prefix-definer (prefix name &rest body)
;;     "Defines a new general definer called my-`name'-definer with prefix
;; `prefix' appended to `leader-prefix', a which-key hint under `name',
;; and the provided `body'."
;;     (declare (indent defun))
;;     (general-create-definer
;;       ;; definer name
;;       (make-symbol (concat "my-" name "-definer"))
;;       ;; prefix key
;;       :prefix (concat leader-prefix " " prefix)
;;       ;; which-key hint
;;       "" (:ignore t :wk ,name)
;;       ;; body
;;       ,@body))

  ;; Declare definers
  (general-create-definer my-leader-def
    :prefix leader-prefix)
  (general-create-definer my-major-def
    :prefix major-prefix))

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

;; File shortcuts
;; (def-file-shortcut "C-c f"
;;   ("i" user-init-file)
;;   ("e" early-init-file)
;;   ("p" "~/projects"))

;;; Package/Features/Modes configuration

;; Garbage collection tweaks
(use-package gcmh
  :hook
  (after-init . gcmh-mode))

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
  (after-init . save-place-mode)
  :init
  (setq save-place-file (expand-file-name "saveplace" my-local-dir)))

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
  ([remap imenu] 'helm-imenu)
  :config
  (setq helm-apropos-fuzzy-match t)

  (use-package helm-xref))

;; Company
(use-package company
  :defer t
  :general
  (:keymaps
   'company-active-map
   "<tab>" 'company-complete-common-or-cycle)
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
(without-system windows-nt
  (use-package tree-sitter
    :if (>= emacs-major-version 29)
    :defer t
    :init
    (when (boundp 'major-mode-remap-alist)
      (setq major-mode-remap-alist
            '((c-mode . c-ts-mode)
              (c++-mode . c++-ts-mode)
              (rust-mode . rust-ts-mode))))
    :config
    (setq treesit-extra-load-path
          (list (expand-file-name "tree-sitter" my-local-dir))
          treesit-language-source-alist
          '((c "https://github.com/tree-sitter/tree-sitter-c")
            (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")))))

;; Avy
(use-package avy
  :general
  ("M-g M-g" 'avy-goto-line)
  ("C-;" 'avy-goto-char)
  ("C-:" 'avy-goto-char-2))

;; Projects
(use-package project
  :config
  (setq project-list-file (expand-file-name "projects" my-local-dir)))

;; EDE
(use-package ede
  :defer t)

;; Snippets
(use-package yasnippet
  :defer t)

;; Lsp client
(without-system 'windows-nt
  (use-package eglot
    :hook
    ((c++-ts-mode . (lambda ()
                      (eglot-ensure)
                      (company-mode 1)
                      (yas-minor-mode 1)))
     (rust-ts-mode . (lambda ()
                       (eglot-ensure)
                       (company-mode 1)
                       (yas-minor-mode 1))))
    :general
    (:keymaps
     'eglot-mode-map
     :prefix "C-,"
     "M-R" 'eglot-reconnect
     "S" 'eglot-shutdown
     "M-S" 'eglot-shutdown-all
     "r" 'eglot-rename
     "f" 'eglot-format
     "F" 'eglot-format-buffer
     "a a" 'eglot-code-actions
     "a o" 'eglot-code-action-organize-imports
     "a q" 'eglot-code-action-quickfix
     "a e" 'eglot-code-action-extract
     "a i" 'eglot-code-action-inline
     "a r" 'eglot-code-action-rewrite
     "C-i" 'eglot-inlay-hints-mode
     "l" 'flymake-show-buffer-diagnostics
     "L" 'flymake-show-project-diagnostics
     "i" 'imenu)
    :config
    (setq eglot-autoshutdown t)))

;; Hungry delete
(use-package smart-hungry-delete
  :general
  ([remap delete-backward-char] 'smart-hungry-delete-backward-char)
  ([remap backward-delete-char-untabify] 'smart-hungry-delete-backward-char)
  ([remap delete-char] 'smart-hungry-delete-forward-char)
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
  :general
  (my-leader-def
    "o e" 'eshell-other-window)
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

  (setq eshell-directory-name (expand-file-name "eshell" my-local-dir)))

;; Server settings
(use-package server
  :defer t
  :config
  (setq server-auth-dir (expand-file-name "server" my-local-dir)))

;;; Programming languages

;; Programming modes
(general-def
  :keymaps 'prog-mode-map
  "C-M-;" 'uncomment-region)

;; Emacs Lisp
(my-major-def
  :keymaps 'emacs-lisp-mode-map
  "m" 'emacs-lisp-macroexpand
  "b" 'eval-buffer
  "r" 'eval-region)

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

;; Rust
(use-package rust-mode
  :defer t)

(use-package cargo
  :hook
  (rust-ts-mode . cargo-minor-mode)
  :general
  (:keymaps
   'cargo-mode-map
   "C-, C-c" 'cargo-minor-mode-command-map))
