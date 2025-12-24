;;; helm-get-file.el --- Get file path from project using Helm -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: User
;; Keywords: files, helm

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Provides `helm-get-file' function to select a file from project directory
;; recursively and return the file path without opening it.
;;

;;; Code:

(require 'helm)
(require 'helm-mode)
(require 'helm-files)

(defvar helm-get-file-find-files-fn
  (cond ((or (executable-find "fd")
             (executable-find "fdfind"))
         #'helm-get-file--fd-find-files)
        ((executable-find "rg")
         #'helm-get-file--rg-find-files)
        (t #'helm-get-file--walk-directory))
  "Function to retrieve files recursively.
A function that takes a directory name as only arg.")

(defun helm-get-file--walk-directory (directory)
  "List files recursively in DIRECTORY using pure Elisp."
  (let ((result nil))
    (dolist (file (directory-files-recursively directory ".*" nil))
      (push file result))
    (nreverse result)))

(defun helm-get-file--fd-find-files (directory)
  "List files recursively in DIRECTORY using fd."
  (let* ((fd-exe (or (executable-find "fdfind")
                     (executable-find "fd")))
         (cmd (format "%s --type f --hidden . %s"
                      fd-exe
                      (shell-quote-argument directory))))
    (with-temp-buffer
      (call-process-shell-command cmd nil t nil)
      (split-string (buffer-string) "\n" t))))

(defun helm-get-file--rg-find-files (directory)
  "List files recursively in DIRECTORY using rg."
  (let ((cmd (format "rg --files --hidden %s"
                     (shell-quote-argument directory))))
    (with-temp-buffer
      (call-process-shell-command cmd nil t nil)
      (split-string (buffer-string) "\n" t))))

(defun helm-get-file--get-project-root ()
  "Get project root directory.
Try `project-current' first, fall back to `default-directory'."
  (or (when-let ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          ;; Emacs < 28 compatibility
          (car (project-roots proj))))
      default-directory))

;;;###autoload
(defun helm-get-file (&optional directory initial-input)
  "Display files in DIRECTORY recursively and return the selected file path.

This function uses Helm to display all files under DIRECTORY
and returns the file path selected by user when they press RET,
WITHOUT opening the file in Emacs.

If DIRECTORY is not provided, it attempts to find the project root using
`project-current', or falls back to `default-directory'.

INITIAL-INPUT is the initial input for helm filtering.

Returns:
  - The absolute path of the selected file as a string.
  - nil if user quits without selecting."
  (interactive)
  (let* ((root (or directory (helm-get-file--get-project-root)))
         (expanded-root (expand-file-name root))
         (files (funcall helm-get-file-find-files-fn expanded-root)))
    (helm-comp-read
     "Select file: "
     files
     :initial-input initial-input
     :name (format "Files in %s" (abbreviate-file-name expanded-root))
     :buffer "*helm-get-file*"
     :must-match t
     :nomark t
     :candidates-in-buffer t)))

;;;###autoload
(defun helm-get-file-relative (&optional directory initial-input)
  "Like `helm-get-file' but returns relative path to DIRECTORY.

If DIRECTORY is not provided, it attempts to find the project root using
`project-current', or falls back to `default-directory'.

INITIAL-INPUT is the initial input for helm filtering.

Returns:
  - The relative path of the selected file as a string.
  - nil if user quits without selecting."
  (interactive)
  (let* ((root (or directory (helm-get-file--get-project-root)))
         (expanded-root (expand-file-name root))
         (selected (helm-get-file expanded-root initial-input)))
    (when selected
      (file-relative-name selected expanded-root))))

(provide 'helm-get-file)

;;; helm-get-file.el ends here

