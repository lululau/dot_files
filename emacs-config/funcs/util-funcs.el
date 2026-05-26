;; -*- lexical-binding: t; -*-

(defun lx/mark-and-yank-whole-buffer ()
  "Mark and yank(copy) whole buffer."
  (interactive)
  (save-excursion
    (progn (call-interactively 'mark-whole-buffer)
           (call-interactively 'evil-yank))))

(defun lx/mark-and-delete-whole-buffer ()
  "Mark and delete whole buffer."
  (interactive)
  (progn (call-interactively 'mark-whole-buffer)
         (call-interactively 'evil-delete)))
