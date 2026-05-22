;; -*- lexical-binding: t; -*-
;; Docker process/image backend overrides (ghostel terminal, SSH docker command).
;; Reload safely with:
;;   emacsclient -e '(load "/Users/liuxiang/.config/emacs-config/package-hooks/docker-backend.el" nil t)'

(with-eval-after-load 'docker-process
  (require 'ghostel)
  (defun docker-run-start-file-process-shell-command (program &rest args)
    "Execute \"PROGRAM ARGS\" and return the process."
    (docker-with-sudo
      (let* ((process-args (-remove 's-blank? (-flatten args)))
             (command-args (s-join " " process-args))
             (command (if (string-match-p "ssh" program)
                          (format "%s '%s'" program command-args)
                        (format "%s %s" program command-args)))
             (default-directory (if (string-match-p "ssh" program)
                                    "~"
                                  default-directory)))
        (when docker-show-messages (message "Running: %s" command))
        (start-file-process-shell-command command (apply #'docker-utils-generate-new-buffer-name program process-args) command))))

  (defun docker-run-async-with-buffer-ghostel (program &optional interactive &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new `ghostel' buffer.
If INTERACTIVE is nil, fall back to shell mode since ghostel is interactive."
    (if (not interactive)
        (apply #'docker-run-async-with-buffer-shell program nil args)
      (unless (fboundp 'ghostel-exec)
        (error "The ghostel package is not installed"))
      (let* ((process-args (-remove 's-blank? (-flatten args)))
             (command (s-join " " (-insert-at 0 program process-args)))
             (buffer-name (apply #'docker-utils-generate-new-buffer-name program process-args))
             (command-parts (split-string-and-unquote command))
             (buffer (get-buffer-create buffer-name)))
        (let ((ghostel-kill-buffer-on-exit nil))
          (switch-to-buffer-other-window buffer)
          (with-current-buffer buffer
            (ghostel-exec buffer (car command-parts) (cdr command-parts)))))))

  (defun docker-run-async-with-buffer (program interactive &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new buffer."
    (let ((default-directory (if (string-prefix-p "/scp:" default-directory)
                                 "~"
                               default-directory)))
      (if docker-run-async-with-buffer-function
          (apply docker-run-async-with-buffer-function program interactive args)
        (apply #'docker-run-async-with-buffer-dispatch
               (docker--terminal-backend)
               program interactive args)))))

(with-eval-after-load 'docker-container
  (require 'ghostel)
  (defun docker-container-ghostel (container)
    "Open `ghostel' in CONTAINER."
    (interactive (list (docker-container-read-name)))
    (require 'ghostel nil 'noerror)
    (if (fboundp 'ghostel)
        (let* ((container-address (format "docker:%s:/" container))
               (file-prefix (let ((ssh-host (seq--elt-safe (s-split " " docker-command) 1)))
                              (if ssh-host
                                  (format "/ssh:%s|" (s-chop-suffix ":" ssh-host))
                                "/")))
               (default-directory (format "%s%s" file-prefix container-address)))
          (zsh-ghostel (docker-utils-generate-new-buffer-name "docker" "ghostel:" default-directory)))
      (error "The ghostel package is not installed"))))

(with-eval-after-load 'docker-image
  (defun docker-image-run-selection (command)
    "Run \"docker image run\" with COMMAND on the images selection."
    (interactive "sCommand: ")
    (docker-utils-ensure-items)
    (let* ((run-args (transient-args 'docker-image-run))
           (docker-command (if (seq-contains-p run-args "-t")
                               (replace-regexp-in-string "^ssh" "ssh -t" docker-command)
                             docker-command)))
      (--each (docker-utils-get-marked-items-ids)
        (docker-run-docker-async-with-buffer-interactive "container" "run" run-args it command)))))
