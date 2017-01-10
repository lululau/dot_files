(with-eval-after-load 'ox-reveal
  (defun org-reveal-export-to-html
      (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer to a reveal.js HTML file."
    (interactive)
    (let* ((org-html-head nil)
           (extension (concat "." org-html-extension))
           (file (org-export-output-file-name extension subtreep))
           (clientfile (org-export-output-file-name (concat "_client" extension) subtreep)))

                                        ; export filename_client HTML file if multiplexing
      (setq client-multiplex nil)
      (setq retfile (org-export-to-file 'reveal file
                      async subtreep visible-only body-only ext-plist))

                                        ; export the client HTML file if client-multiplex is set true
                                        ; by previous call to org-export-to-file
      (if (eq client-multiplex t)
          (org-export-to-file 'reveal clientfile
            async subtreep visible-only body-only ext-plist))
      (cond (t retfile)))))
