(defun lx/open-file-in-lnav/get-command (files)
  (let* ((first (car files))
         (host (replace-regexp-in-string "^/scp:\\([^:]*\\):.*" "\\1" first))
         (is-remote (not (string= first host)))
         (is-gzip (string-match-p ".gz$" first)))
    (if is-remote
        (progn
          (setq files (mapcar (lambda (f) (replace-regexp-in-string "^/scp:\\([^:]*\\):" "" f)) files))
          (if is-gzip
              (format "ssh %s 'cat %s' | gunzip -c | lnav -q" host (s-join " " (mapcar (lambda (f) (shell-quote-argument f)) files)))
            (format "ssh %s 'cat %s | gzip -c ' | gunzip -c | lnav -q" host (s-join " " (mapcar (lambda (f) (shell-quote-argument f)) files)))))
      (format "lnav %s" (s-join " " (mapcar (lambda (f) (shell-quote-argument f)) files))))))

(defun lx/open-file-in-lnav/run (files)
  (let* ((default-directory "~/")
         (cmd (lx/open-file-in-lnav/get-command files))
         (script (format "osascript -e \"tell app \\\"iTerm\\\"\nactivate\ntell current session of first window to write text \\\"%s\\\"\nend tell\"" cmd)))
    (shell-command script)))

(defun lx/open-file-in-lnav ()
  (interactive)
  (lx/open-file-in-lnav/run (dired-get-marked-files)))

(provide 'open-file-in-lnav)
