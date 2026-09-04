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

;;;###autoload
(defun spacemacs/copy-file-or-region-for-agent ()
  "Copy current file path or active region formatted for coding agents to clipboard.
If the region is not active, copies @ABSOLUTE_FILE_PATH.
Otherwise copies \"ABSOLUTE_FILE_PATH 中的第 M-N 行:\n\nSELECTION\n\".
After copying, deactivate the region when applicable."
  (interactive)
  (let ((file-path (or (buffer-file-name)
                       (and (derived-mode-p 'dired-mode)
                            (dired-get-filename nil t)))))
    (unless file-path
      (user-error "Current buffer is not visiting a file"))
    (let* ((abs-path (expand-file-name file-path))
           (had-region (use-region-p))
           (str
            (if had-region
                (let* ((beg (region-beginning))
                       (end (region-end))
                       (lo (min beg end))
                       (hi (max beg end))
                       (hi-line-pos (max lo (1- hi)))
                       (m (save-excursion (goto-char lo) (line-number-at-pos)))
                       (n (save-excursion (goto-char hi-line-pos) (line-number-at-pos)))
                       (selection (buffer-substring-no-properties lo hi)))
                  (concat abs-path " 中的第 "
                          (number-to-string m) "-" (number-to-string n)
                          " 行:\n\n" selection "\n"))
              (concat "@" abs-path))))
      (kill-new str)
      (when had-region
        (deactivate-mark))
      (message "%s" str))))

;;;###autoload
(defalias 'spacemacs/copy-for-agent #'spacemacs/copy-file-or-region-for-agent
  "Shorthand alias for `spacemacs/copy-file-or-region-for-agent'.")
