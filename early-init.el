;;; -*- lexical-binding: t -*-

;; Change the default location of native comp files
(when (native-comp-available-p)
  (setcar native-comp-eln-load-path
	  (expand-file-name (concat user-emacs-directory "local/eln-cache/"))))

;; Disable tool, menu, and scroll bars before GUI loads to prevent flash
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable package.el initialization because this config uses
;; straight.el as its package manager
(setq package-enable-at-startup nil)


