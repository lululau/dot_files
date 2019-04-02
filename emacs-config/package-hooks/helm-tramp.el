(with-eval-after-load 'helm-tramp
  (defun helm-tramp ()
    "Open your ~/.ssh/config with helm interface.
You can connect your server with tramp"
    (interactive)
    (unless (file-exists-p "~/.ssh/config")
      (error "There is no ~/.ssh/config"))
    (run-hooks 'helm-tramp-pre-command-hook)
    (helm :sources '(helm-tramp--source) :buffer "*helm tramp*")
    (run-hooks 'helm-tramp-post-command-hook))

  (defun helm-tramp--candidates (&optional file)
    "Collect candidates for helm-tramp from FILE."
    (let ((source (split-string
                   (with-temp-buffer
                     (insert-file-contents (or file "~/.ssh/config"))
                     (buffer-string))
                   "\n"))
          (hosts (if file '() helm-tramp-custom-connections)))
      (dolist (host source)
        (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
          (setq host (match-string 1 host))
          (if (string-match "[ \t\n\r]+\\'" host)
              (replace-match "" t t host))
          (if (string-match "\\`[ \t\n\r]+" host)
              (replace-match "" t t host))
          (unless (string= host "*")
            (if (string-match "[ ]+" host)
                (let ((result (split-string host " ")))
                  (while result
                    (push
                     (concat "/" tramp-default-method ":" (car result) ":")
                     hosts)
                    (push
                     (concat "/ssh:" (car result) "|sudo:root@" (car result) ":/")
                     hosts)
                    (pop result)))
              (push
               (concat "/" tramp-default-method ":" host ":")
               hosts)
              (push
               (concat "/ssh:" host "|sudo:" host ":/")
               hosts))))
        (when (string-match "Include +\\(.+\\)$" host)
          (setq include-file (match-string 1 host))
          (when (not (file-name-absolute-p include-file))
            (setq include-file (concat (file-name-as-directory "~/.ssh") include-file)))
          (when (file-exists-p include-file)
            (setq hosts (append hosts (helm-tramp--candidates include-file))))))
      (push (concat "/sudo:root@localhost:" helm-tramp-localhost-directory) hosts)
      (reverse hosts))))
