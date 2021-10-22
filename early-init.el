;;; early-init.el -*- lexical-binding: t; -*-

;; Disable garbage collection during startup to decrease startup
;; times. It is reset later with `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;; Disable tool, menu, and scroll bars before GUI loads to prevent flash
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable package.el initialization because this config uses
;; straight.el as its package manager
(setq package-enable-at-startup nil)

;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun my-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'my-reset-file-handler-alist-h 101)))

;; Start Emacs in the directory of this file
(setq default-directory (file-name-directory load-file-name))
