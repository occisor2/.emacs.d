;;; -*- lexical-binding: t -*-

(use-package dash)

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun font-exists-p (font)
  "Check if font is available"
  (if (null (x-list-fonts font)) nil t))

(defmacro def-shortcut (prefix &rest body)
(defmacro def-file-shortcut (prefix &rest body)
  "A definer for file shorcuts.

Shortcut keybinds are appending to `prefix' and are given a which-key
hint that is the file name.

The prefix should be a valid emacs prefix.
Body syntax:
(def-shortcut prefix
  ([Keybind string] [File name])
  ...)"
  (declare (indent defun))
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
