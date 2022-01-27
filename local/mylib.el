;;; mylib.el -*- lexical-binding: t; -*-

(defun my-kill-line-if-no-region (beg end)
  "If the region is not active, kill the current line; otherwise
kill the current region."
  (interactive "r")
  (if (use-region-p)
      (kill-region beg end t)
    (progn
      (push (point) buffer-undo-list)
      (kill-region (line-beginning-position) (line-beginning-position 2))
      (back-to-indentation))))

;; https://github.com/bbastsov/crux
(defun my-open-line-above (N)
  "Inserts N empty lines above the current line. Position the
cursor at its beginning, according to the current mode."
  (interactive "p")
  (move-beginning-of-line nil)
  (newline-and-indent N)
  (forward-line (- N))
  (indent-according-to-mode))

(provide 'mylib)
