;;; -*- lexical-binding: t -*-

;; Change the default location of native comp files
(when (native-comp-available-p)
  (let ((cache-dir (expand-file-name "local/eln-cache/"
                                     user-emacs-directory)))
    (if (boundp 'startup-redirect-eln-cache)
        (startup-redirect-eln-cache cache-dir)
      (setcar native-comp-eln-load-path
	          (expand-file-name cache-dir))))
  (setq native-comp-async-report-warnings-errors 'silent))

;; Disable tool, menu, and scroll bars before GUI loads to prevent flash
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable package.el initialization because this config uses
;; straight.el as its package manager
(setq package-enable-at-startup nil)


