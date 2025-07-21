;;; -*- lexical-binding: t -*-

(use-package dash)
(use-package mmt)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent 1))
  `(when (eq system-type ',type)
     ,@body))

(defmacro without-system (type &rest body)
  "Evaluate BODY if `system-type' does not equal TYPE."
  (declare (indent 1))
  `(unless (eq system-type ',type)
     ,@body))

(defun font-exists-p (font)
  "Check if font is available"
  (if (null (x-list-fonts font)) nil t))

(defmacro command-wrap (&rest body)
  "Wraps `body' in an interactive lambda"
  (declare (indent defun))
  `(lambda ()
     (interactive)
     ,@body))

(defmacro my-def-shortcut (&body body)
  "Creates keybindings for jumping to common files.

(my-def-shorcut
 \"key binding\" \"path\")"
  (declare (indent 2))
  (unless (and (cl-evenp (length body))
               (not (zerop (length body))))
    (error "malformed file shortcut body"))
  `(progn
     ,@(cl-loop for (path shortcut) on body by #'cddr while shortcut
                collect `(global-set-key (kbd ,shortcut) (lambda ()
                                                           (interactive)
                                                           (find-file ,path))))
     t))

(provide 'my-misc)
