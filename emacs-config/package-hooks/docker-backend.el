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

  (defun docker-container--detect-shell (container)
    "Return the best available shell in CONTAINER, preferring zsh > bash > sh."
    (docker-with-sudo
      (let* ((script "for s in zsh bash sh; do p=$(command -v $s 2>/dev/null); if [ -n \"$p\" ]; then echo $p; exit 0; fi; done; echo /bin/sh")
             (process-args (append (docker-arguments)
                                   (list "exec" container "sh" "-c" script)))
             (command-args (mapconcat #'shell-quote-argument process-args " "))
             (command (if (string-match-p "ssh" docker-command)
                          (format "%s '%s'" docker-command command-args)
                        (format "%s %s" docker-command command-args)))
             (result (string-trim (shell-command-to-string command))))
        (when docker-show-messages
          (message "Using shell %s in container %s" result container))
        (if (string-empty-p result)
            docker-container-shell-file-name
          result))))

  (defun docker-container--inspect-config (container)
    "Return the Config alist for CONTAINER via synchronous docker inspect."
    (docker-with-sudo
      (let* ((process-args (append (docker-arguments) (list "inspect" container)))
             (command-args (mapconcat #'shell-quote-argument process-args " "))
             (command (if (string-match-p "ssh" docker-command)
                          (format "%s '%s'" docker-command command-args)
                        (format "%s %s" docker-command command-args))))
        (cdr (assq 'Config (aref (json-read-from-string (shell-command-to-string command)) 0))))))

  (defun docker-container--apply-remote-shell (container-shell)
    "Configure TRAMP and shell to use CONTAINER-SHELL on `default-directory'."
    (tramp-set-connection-property default-directory "remote-shell" container-shell)
    (with-connection-local-variables
      (setq-connection-local
       tramp-remote-shell container-shell
       shell-file-name container-shell
       explicit-shell-file-name container-shell)))

  (defun docker-container-shell-env-auto (container)
    "Open `ghostel' in CONTAINER with env and auto-detected shell (zsh > bash > sh)."
    (interactive (list (docker-container-read-name)))
    (docker-container-assert-tramp-docker)
    (require 'zsh-ghostel nil 'noerror)
    (unless (fboundp 'zsh-ghostel--internal)
      (error "The ghostel package is not installed"))
    (let* ((container-shell (docker-container--detect-shell container))
           (container-address (format "%s:%s:" docker-container-tramp-method container))
           (file-prefix (let ((prefix (file-remote-p default-directory)))
                          (if prefix
                              (format "%s|" (s-chop-suffix ":" prefix))
                            "/")))
           (container-config (docker-container--inspect-config container))
           (container-workdir (cdr (assq 'WorkingDir container-config)))
           (container-env (cdr (assq 'Env container-config)))
           (default-directory (format "%s%s%s" file-prefix container-address container-workdir))
           (tramp-remote-process-environment
            (append container-env (list (format "SHELL=%s" container-shell)) nil))
           (buffer-name (docker-utils-generate-new-buffer-name "docker" "ghostel-env-auto:" default-directory))
           ;; ghostel-tramp-shells hardcodes docker => /bin/sh; override per container.
           (ghostel-tramp-shells (cons (list "docker" container-shell)
                                       (assq-delete-all "docker" ghostel-tramp-shells))))
      (docker-container--apply-remote-shell container-shell)
      (let ((ghostel-kill-buffer-on-exit nil))
        (zsh-ghostel--internal #'pop-to-buffer buffer-name))))

  (defun docker-container-shell-env-auto-selection ()
    "Run `docker-container-shell-env-auto' on the containers selection."
    (interactive)
    (docker-utils-ensure-items)
    (--each (docker-utils-get-marked-items-ids)
      (docker-container-shell-env-auto it)))

  (defun docker-container-ghostel (container)
    "Open `ghostel' in CONTAINER."
    (interactive (list (docker-container-read-name)))
    (require 'ghostel nil 'noerror)
    (if (fboundp 'ghostel)
        (let* ((container-address (format "docker:%s:/" container))
               (file-prefix (let ((ssh-host (seq--elt-safe (s-split " " docker-command) 1)))
                              (if ssh-host
                                  (format "/rpc:%s|" (s-chop-suffix ":" ssh-host))
                                "/")))
               (default-directory (format "%s%s" file-prefix container-address)))
          (zsh-ghostel (docker-utils-generate-new-buffer-name "docker" "ghostel:" default-directory)))
      (error "The ghostel package is not installed")))

  (evilified-state-evilify-map docker-container-mode-map :mode docker-container-mode :bindings (kbd "RET") 'docker-container-shell-env-auto-selection))

(with-eval-after-load 'docker-image
  (defun lx/docker-image-run-selection (command)
    "Run \"docker container run -i -t --rm COMMAND\" on the images selection.
Skips the `docker-image-run' transient menu."
    (interactive "sCommand: ")
    (docker-utils-ensure-items)
    (let* ((run-args '("-i" "-t" "--rm"))
           (docker-command (if (seq-contains-p run-args "-t")
                               (replace-regexp-in-string "^ssh" "ssh -t" docker-command)
                             docker-command)))
      (--each (docker-utils-get-marked-items-ids)
        (docker-run-docker-async-with-buffer-interactive "container" "run" run-args it command))))

  (defun docker-image-run-selection (command)
    "Run \"docker image run\" with COMMAND on the images selection."
    (interactive "sCommand: ")
    (docker-utils-ensure-items)
    (let* ((run-args (transient-args 'docker-image-run))
           (docker-command (if (seq-contains-p run-args "-t")
                               (replace-regexp-in-string "^ssh" "ssh -t" docker-command)
                             docker-command)))
      (--each (docker-utils-get-marked-items-ids)
        (docker-run-docker-async-with-buffer-interactive "container" "run" run-args it command))))
  (evilified-state-evilify-map docker-image-mode-map :mode docker-image-mode :bindings (kbd "RET") 'lx/docker-image-run-selection))
