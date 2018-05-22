(defun lx/capture-note (title link body)
  (let* ((filename (format "~/Documents/notes/webclips/%s.org" (replace-regexp-in-string "/" "_" (org-link-unescape title))))
         (unescaped-link (org-link-unescape link))
        (buffer (find-file filename)))
    (with-current-buffer buffer
      (insert (org-link-unescape body))
      (if (not (zerop (call-process-region
                       (point-min) (point-max)
                       "~/bin/html2org" t t nil title unescaped-link)))
          (message "Pandoc failed: %s" (buffer-string))
        (progn
          ;; Pandoc succeeded
          (org-mode)
          (outline-show-all)
          (spacemacs/indent-region-or-buffer)
          (mark-whole-buffer)
          (call-interactively 'org-fill-paragraph)
          (deactivate-mark)
          (save-buffer)
          t)))))
