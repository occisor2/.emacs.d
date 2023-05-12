;;; -*- lexical-binding: t -*-

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun font-exists-p (font)
  "Check if font is available"
  (if (null (x-list-fonts font)) nil t))

(provide 'my-misc)
