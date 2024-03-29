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

(defmacro def-file-shortcut (prefix &rest body)
  "A definer for file shorcuts.

Shortcut keybinds are appending to `prefix' and are given a which-key
hint that is the file name.

The prefix should be a valid emacs prefix.
Body syntax:
(def-shortcut prefix
  ([Keybind string] [File name])
  ...)"
  (declare (indent 1))
  `(general-def
     :prefix ,prefix
     ,@(-flatten-n
        1
        (cl-loop
         for x in body collect
         (list
          (car x)
          `'((lambda ()
               (interactive)
               (find-file ,(cadr x)))
             :which-key ,(file-name-nondirectory (cadr x))))))))

(provide 'my-misc)
