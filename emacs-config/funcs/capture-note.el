(defun lx/nbsp-to-space (s)
  "Convert HTML non-breaking spaces to plain spaces in S."
  ;; Not sure why sometimes these are in the HTML and Pandoc converts
  ;; them to underlines instead of spaces, but this fixes it.
  (replace-regexp-in-string (rx "&nbsp;") " " s t t))

(defun lx/conform-filename (filename)
  (replace-regexp-in-string "^_" "" (replace-regexp-in-string "[[:punct:] \t]\+" "_" filename)))

(defun lx/refine-web-string (str)
  (lx/nbsp-to-space (string-trim (org-link-decode str))))

(defun lx/refine-filename (filename)
  (lx/conform-filename (lx/refine-web-string filename)))

(defun lx/capture-note (title link body)
  (let* ((filename (format "~/Documents/notes/webclips/%s.org" (lx/refine-filename title)))
         (refined-title (lx/refine-web-string title))
         (refined-link (lx/refine-web-string link))
         (buffer (find-file filename)))
    (with-current-buffer buffer
      (insert (lx/refine-web-string body))
      (call-process-region
       (point-min) (point-max)
       "~/bin/html2org" t t nil refined-title refined-link)
      (org-mode)
      (outline-show-all)
      (spacemacs/indent-region-or-buffer)
      (mark-whole-buffer)
      (call-interactively 'org-fill-paragraph)
      (deactivate-mark)
      (save-buffer)
      t)))

(defun lx/download-org-images ()
  (interactive)
  (with-current-buffer (current-buffer)
    (call-process-region (point-min) (point-max)
                         "~/bin/download-org-images" t t nil (buffer-file-name))
    (org-toggle-inline-images)))
