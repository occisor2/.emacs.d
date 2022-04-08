;;; init.el -*- lexical-binding: t; -*-

;; Global variables
(defvar my-local-dir (expand-file-name "local/" user-emacs-directory))
(defvar my-cache-dir (expand-file-name "cache/" user-emacs-directory))

;;; Package management

;; Bootstrap straight.el
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

;; Required packages
(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(straight-use-package 'diminish)

;; load my personal library
(add-to-list 'load-path (expand-file-name "local" default-directory))
(require 'mylib)

;;; Keybinding

;; which-key.el for displaying binds
(use-package which-key
  :diminish which-key-mode
  :hook
  ((after-init . which-key-mode)))

;; general.el for making binds more convenient
(use-package general
  :config
  ;; Leader key
  (defconst my-leader "C-c")
  
  ;; Custom definers
  (general-create-definer my-leader-def
    :prefix my-leader
    "" '(:ignore t :wk "leader")
    "z" '(:ignore t :wk "text")
    "b" '(:ignore t :wk "buffer")
    "u" '(:ignore t :wk "ui")
    "w" '(:ignore t :wk "window")
    "o" '(:ignore t :wk "open")
    "f" '(:ignore t :wk "file"))

  (general-create-definer my-major-mode-def
    :prefix (concat my-leader " m")
    "" '(:ignore t :wk "major mode")))

;; File shortcuts
(my-filesc-def
 "p" "~/projects/"
 "i" "~/.emacs.d/init.el")

;; hydra for making chords
(use-package hydra
  :general
  (my-leader-def
    "w r" '(hydra-my-resize/body :wk "resize"))
  :config
  (defhydra hydra-my-resize (:color red)
    ("k" enlarge-window "enlarge vertically")
    ("j" shrink-window "shrink vertically")
    ("h" shrink-window-horizontally "shrink horizontally")
    ("l" enlarge-window-horizontally "enlarge horizontally")
    ("q" nil "quit")))

;; Bind my custom editing functions
(general-def
  [remap kill-region] 'my-kill-line-if-no-region
  "M-o" 'my-open-line-above
  "C-x C-b" 'ibuffer)

;;; Editor settings

;; Store custom file in `my-local-dir'
(setq custom-file (concat my-local-dir "custom.el"))

;; Load settings from `custom-file'
(when (file-exists-p custom-file)
  (load custom-file))

;; Allow help functions to autoload functions to get doc strings
(setq help-enable-symbol-autoload t)

;; ;; Only report errors in wanring buffers
;; (setq warning-minimum-level :error)

;; Native comp
(setq native-comp-async-report-warnings-errors nil)

;; Quiet startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; in a daemon session where it'll say "Starting Emacs daemon." instead
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Follow symlinks
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable same file warnings
(setq find-file-suppress-same-file-warnings t)

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

;; Configure auto-saves
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat my-cache-dir "autosave/")
      tramp-auto-save-directory  (concat my-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Indention and tabs
(setq-default indent-tabs-mode nil
	          tab-width 4)

;; Indent to line's indention otherwise literally indent but not in prog modes
(setq-default tab-always-indent nil)
(add-hook 'prog-mode-hook #'(lambda () (setq-local tab-always-indent t)))

;; Continuation and truncation
(setq-default word-wrap t      ; wrap words at whitespace
              truncate-lines t ; disable wrapping and use truncation
              truncate-partial-width-windows nil)

;; Disable sentences ending in double spaces
(setq sentence-end-double-space nil)

;; Require final newlines always to conform to POSIX
(setq require-final-newline t)

;; Add newlines with C-n
;; (setq next-line-add-newlines t)

;; Define and bind a few commands for making using the mark in tmm better
(defun my-push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun my-jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun my-exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(general-def
  "C-`" 'my-push-mark-no-activate
  "M-`" 'my-jump-to-mark
  [remap exchange-point-and-mark] 'my-exchange-point-and-mark-no-activate)
  

;;; Optimizations

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

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font.
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Inhibits fontification while receiving input, which should help a little
;; with scrolling performance
(setq redisplay-skip-fontification-on-input t)

;; Garbage collection
(use-package gcmh
  :diminish gcmh-mode
  :hook
  ((after-init . gcmh-mode))
  :config
  (setq gcmh-idle-delay 0.5
	    gcmh-high-cons-threshold (* 16 1024 1024)))

;;; UI

;; Set font
(defvar my-default-font "DejaVuSansMono Nerd Font-10"
  "The default font my init file loads.")
(when (font-exists-p my-default-font)
  (set-frame-font my-default-font nil t))

;; Unset variables so function calls work
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Disable tooltips
(tooltip-mode 0)

;; Start with the frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Change frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Smooth scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Minibuffer settings
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02
      resize-mini-windows t
      read-minibuffer-restore-windows t)

;; Try to keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Make yes/no prompts y/n
(setq use-short-answers t)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Do no stretch cursor on wide characters
(setq x-stretch-cursor nil)

;; Disable the bell
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Show current key-sequence in minibuffer
(setq echo-keystrokes 0.02)

;; Paste at point not mouse click
(setq mouse-yank-at-point t)

;; Set `fill-column' to 80
(setq-default fill-column 80)

;; Show `fill-column' in prog modes
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(my-leader-def
  "u f" '(display-fill-column-indicator-mode :wk "fill-column indicator"))

;; Modeline
(column-number-mode t)

;; Line numbers
(use-package display-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode))
  :general
  (my-leader-def
    "u l" '(display-line-numbers-mode :wk "line nums")))

;; Show whitespace
(use-package whitespace
  :general
  (my-leader-def
    "u w" '(whitespace-mode :wk "whitespace")))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package gruvbox-theme
  :disabled
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package zenburn-theme
  :disabled
  :config
  (load-theme 'zenburn t))

(use-package nano-theme
  :disabled
  :config
  (load-theme 'nano-dark t))

(use-package darktooth-theme
  :disabled
  :config
  (load-theme 'darktooth t))

;; Highlight the current line in prog modes
(use-package hl-line
  :hook
  ((prog-mode . hl-line-mode))
  :general
  (my-leader-def
    "u h" '(hl-line-mode :wk "hl-line")))

;; Highlight parens in prog modes
(use-package paren
  :hook
  ((prog-mode . show-paren-mode))
  :general
  (my-leader-def
    "u p" '(show-paren-mode :wk "hl-parens"))
  :config
  (setq show-paren-delay 0.0))

;; Color code sexps
(use-package rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode))
  :general
  (my-leader-def
    "u r" '(rainbow-delimiters-mode :wk "rainbow delims")))

;; Blink cursor on window switch
(use-package beacon
  :diminish
  :hook
  ((after-init . beacon-mode)))

;;; Windows

;; Default window split functions do not switch to the new window so fix that
(defun my-vsplit ()
  "Split the current window vertically then switch to the new window"
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun my-hsplit ()
  "Split the current window vertically then switch to the new window"
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(my-leader-def
  "w v" '(my-vsplit :wk "split vertically")
  "w h" '(my-hsplit :wk "split horizontally"))

;; Quickly jump to or manipulate any displayed window
(use-package ace-window
  :general
  (general-def
    "C-x o" 'ace-window))

;; Shrink border betwen windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;;; Misc. features and third party packages

;; Abbrevs
(setq abbrev-file-name (expand-file-name "abbrev.el" my-local-dir)
      save-abbrevs 'silently)
(add-hook 'after-init-hook #'abbrev-mode)
(diminish 'abbrev-mode)

;; Auto Insert
;; (use-package autoinsert)

;; Keep track of recently opened files
(use-package recentf
  :hook
  ((after-init . recentf-mode)
   (kill-emacs . recentf-cleanup))
  :config
  (setq recentf-save-file (concat my-cache-dir "recentf")
        recentf-auto-cleanup nil
        recentf-max-saved-items 100)

  ;; Text properties inflate the size of recentf's files
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; The most sensible time to clean up your recent files list is when you quit
  ;; Emacs (unless this is a long-running daemon session).
  (setq recentf-auto-cleanup (if (daemonp) 300)))

;; Save point of a file across sessions
(use-package saveplace
  :hook
  ((after-init . save-place-mode)
   (save-place-find-file . recenter))
  :config
  (setq save-place-file (concat my-cache-dir "saveplace")))

;; Jump around the buffer easily
(use-package avy
  :general
  (general-def
    "M-'" 'avy-goto-char-timer
    "M-g M-g" 'avy-goto-line))

;; Automatically pair open sexps
(use-package elec-pair
  :hook
  ((prog-mode . electric-pair-mode)))         

;; Delete whitespace in one delete
(use-package smart-hungry-delete
  :general
  ("<backspace>" 'smart-hungry-delete-backward-char
   "C-d" 'smart-hungry-delete-forward-char))

;; Helm. Enough said
(use-package helm
  :hook
  ((after-init . helm-mode))
  :general
  (general-def
    "M-x" 'helm-M-x
    "M-y" 'helm-show-kill-ring
    "C-s" 'helm-occur
    [remap find-file] 'helm-find-files
    "C-h a" 'helm-apropos
    "C-h SPC" 'helm-all-mark-rings
    "C-x b" 'helm-mini)

  (general-def
    :keymaps 'helm-mode-map
    "<tab>" 'helm-execute-persistent-action
    "C-z" 'helm-select-action)

  (my-leader-def
    "b i" '(helm-imenu :wk "imenu"))
  :config
  (setq helm-autoresize-max-height 40
        helm-autoresize-min-height 15)

  (helm-autoresize-mode 1))

(use-package helm-xref
  :after helm)

;; Terminal emulation
(use-package vterm
  :defer t)

(use-package vterm-toggle
  :defer t
  :general
  (my-leader-def
    "o t" 'vterm-toggle-cd)
  :config
  (setq vterm-toggle-cd-auto-create-buffer nil))

;; An amazing calculator
(use-package calc
  :defer t
  :general
  (general-def
    :keymaps 'calc-mode-map
    "C-<tab>" 'calc-roll-up)
  :config
  (setq calc-settings-file (concat my-local-dir "calc-init.el")))

;; Nice autocompletion framework
(use-package company
  :diminish company-mode
  :general
  (general-def
    :keymaps 'company-active-map
    "<tab>" 'company-complete-common-or-cycle
    "<backtab>" 'company-select-previous)
  (my-leader-def
    "u c" '(company-mode :wk "autocompletion"))
  :config
  (setq company-show-numbers t
        company-selection-wrap-around t
        company-idle-delay 0.02
        company-minimum-prefix-length 2)
  (setq-default company-backends
                '(company-files
                  company-capf
                  company-keywords
                  company-dabbrev-code
                  company-dabbrev)))

;; Control popup unruly windows
(use-package shackle
  :diminish
  :hook
  ((after-init . shackle-mode))
  :config
  (setq shackle-rules
        '((compilation-mode :select t))))

;; Visualize indention
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :general
  (my-leader-def
    "u i" '(highlight-indent-guides-mode :wk "indent guides"))
  :config
  (setq highlight-indent-guides-delay 0.2
        highlight-indent-guides-method 'bitmap))

;; Project management
(use-package projectile
  :hook
  ((after-init . projectile-mode))
  :general
  (general-def
    "C-c m p" '(:keymap projectile-command-map :wk "projects"))
  :config
  (setq projectile-require-project-root t
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recently-active
        projectile-enable-caching t
        projectile-cache-file (concat my-cache-dir "projectile.cache")
        projectile-known-projects-file (concat my-cache-dir "projectile.projects")))

;; Outlining, TODO lists, and more
(use-package org
  :defer t
  :diminish org-indent-mode
  :hook
  ((org-mode . (lambda ()
                 (org-indent-mode t)
                 (auto-fill-mode t))))
  :config
  (setq org-catch-invisible-edits 'smart))

;; Snippets
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode)

;; Syntax checking
(use-package flycheck
  :defer t
  :diminish)

;; Tree Sitter
(use-package tree-sitter
  :defer t)

(use-package tree-sitter-langs
  :defer t)

;; Language server protocol client
(use-package lsp-mode
  :hook
  ((lsp-mode . (lambda ()
                 (lsp-enable-which-key-integration)
                 (yas-minor-mode)
                 (setq-local company-transformers nil))))
  :init
  (setq lsp-keymap-prefix "C-c m l")
  :config
  (setq lsp-idle-delay 0.05
        lsp-log-io nil
        lsp-auto-execute-action nil
        lsp-eldoc-render-all nil
        lsp-enable-on-type-formatting nil
        lsp-keep-workspace-alive nil
        lsp-session-file (concat my-cache-dir "lsp.cache")
        ;; Completion
        lsp-completion-enable t
        lsp-completion-filter-on-imcomplete nil
        lsp-completion-sort-initial-results nil
        ;; Headerline
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        ;; Lens
        lsp-lens-enable nil
        ;; Semantic tokens
        lsp-semantic-tokens-enable nil
        lsp-semantic-tokens-honor-refresh-requests t))

;; Manage Git repos directly from Emacs
(use-package magit
  :defer t)

(use-package transient
  :defer t
  :config
  (setq transient-history-file (concat my-cache-dir "transient.history")))

;;;; Programming languages

;;; Emacs Lisp
;;; TODO: define and bind a toggle function for ielm

(defun my-emacs-lisp-mode-hook ()
  (setq-local company-backends
              '(company-files
                company-capf
                company-keywords)
              lisp-indent-function #'lisp-indent-function)
  (company-mode t))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

(use-package lisp-extra-font-lock
  :hook
  ((emacs-lisp-mode . lisp-extra-font-lock-mode)))

(my-major-mode-def
  :keymaps 'emacs-lisp-mode-map
  "e" '(:ignore t :wk "eval")
  "e b" '(eval-buffer :wk "buffer")
  "e r" '(eval-region :wk "region")
  "j" '(:ignore t :wk "jump")
  "j d" '(xref-find-definitions :wk "defintion")
  "j r" '(xref-find-references :wk "reference"))

;;; CC mode (C, C++, Java, AWK)

(use-package cc-mode
  :defer t
  :hook
  ((c-mode-common . (lambda ()
                      ;; Common settings across all cc-modes
                      (auto-fill-mode 1) ; Auto fill long lines
                      (c-toggle-auto-newline 1) ; Automatically insert newlines

                      ;; Syntax highlighting
                      (require 'tree-sitter)
                      (require 'tree-sitter-langs)
                      (tree-sitter-mode 1)
                      (tree-sitter-hl-mode 1)
                      ))
   (c++-mode . (lambda ()
                 (setq-local c-macro-preprocessor
                             (concat "g++ -E -C " buffer-file-name)))))
  :general
  (general-def
    :keymaps 'c-mode-map
    "C-d" 'smart-hungry-delete-forward-char)
  :config
  (defun c-semi&comma-no-newlines-before-nonblanks ()
    "Do not add newlines after semicolons when followed by a non-newline."
    (save-excursion
      (if (and (= (c-last-command-char) ?\;)
	           (zerop (forward-line 1))
	           (bolp) ; forward-line has funny behavior at eob.
	           (not (looking-at "^[ \t]*$")))
	      'stop
        nil)))

  ;; (defun my-c-macro-expand (beg end)
  ;;   "If the region is active, expand macros in the region. If not, then expand
  ;;    every macro in the file, excluding includes."
  ;;   (interactive "r")
  ;;   (if (use-region-p)
  ;;       (c-macro-expand beg end nil)
  ;;     (save-excursion
  ;;       (let ((end-of-includes-pos (re-search-forward "#include <.*>" 20)))
  ;;         (c-macro-expand end-of-includes-pos point-max))
  ;;       (message "worked"))))

  ;; C specific settings
  (defconst my-c-style
    '((c-basic-offset . 4)
      (c-tab-always-indent . t)
      (c-offsets-alist . ((substatement-open . 0)))
      (c-hanging-semi&comma-criteria
       . (c-semi&comma-no-newlines-before-nonblanks))
      (c-cleanup-list . (defun-close-semi list-close-comma))
      (c-hanging-braces-alist
       . ((brace-list-open after)
          (brace-entry-open)))))

  (c-add-style "personal" my-c-style)

  ;; Default styles for each language I use
  (setq c-default-style '((c-mode . "personal")
                          (c++-mode . "personal")
                          (awk-mode . "awk"))))

(use-package ccls
  :hook
  (((c-mode c++-mode) . (lambda ()
                          (require 'ccls)
                          (lsp-deferred)))))

;;; Python
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;;; Common Lisp
(use-package sly
  :defer t
  :hook
  ((sly-mrepl-mode . (lambda ()
                       (setq-local company-minimum-prefix-length 3)
                       (company-mode 1))))
  :init
  (setq inferior-lisp-program "sbcl"
        sly-common-lisp-style-default "modern"))

(use-package lisp-mode
  :straight nil
  :hook
  ((lisp-mode . (lambda ()
                  (company-mode t)
                  (setq-local company-minimum-prefix-length 3)
                  (sly)))))
