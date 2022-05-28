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

(defun font-exists-p (font)
  "Predicate for whether a font is installed on the host system."
  (if (null (x-list-fonts font))
      nil
    t))

(defmacro my-wrap-command (&rest body)
  "Wrap forms in an interactive lambda."
  `(lambda ()
     (interactive)
     ,@body))

;; (defmacro my-create-fileshortcuts (&rest body)
;;   ;; (cl-loop
;;   ;;  repeat (/ (length lst) 2)
;;   ;;  for a = lst then (cddr a)
;;   ;;  and b = (cdr lst) then (cddr b)
;;   ;;  collect (list (car a) (car b)))
;;   (push 'my-leader-def
;;         (cl-loop
;;          for i to (length body)
;;          for x in body
;;          if (cl-evenp i)
;;          collect `(,@(concat "f " x)) into shortcuts
;;          else
;;          collect `(quote
;;                    (,(my-wrap-command
;;                      (find-file (expand-file-name x)))
;;                     :wk ,(file-name-nondirectory x)))
;;          into shortcuts
;;          finally (return shortcuts))))

;; (my-create-fileshortcuts
;;  "s" "~/.bashrc")

(defmacro my-filesc-def (&rest body)
  "A wrapper over my-leader-def that elimates the need for boiler plate code for
defining lambdas around find-file commands.

This macro is absolutely disgusting and inefficeint. I don't know
much about macros, so this was some practice at understanding them.
This will get cleaned up at some point, but it works for now."
  (cl-do* ((body-len (length body))
           (sc-list nil   `(,(concat "f " (nth index body))
                             (quote
                              ((lambda ()
                                 (interactive)
                                 (find-file
                                  ,(expand-file-name (nth (1+ index)
                                                          body))))
                               :wk ,(file-name-nondirectory (nth (1+ index)
                                                                 body))))))
           (forms (list 'my-leader-def) (dolist (x sc-list forms)
                                          (push x forms)))
           (index 0 (+ 2 index)))
          ((>= index body-len) (nreverse forms))))

(provide 'mylib)
