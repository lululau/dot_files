(spacemacs|use-package-add-hook docker
  :post-config
  (progn
    (evilified-state-evilify-map docker-image-mode-map :mode docker-image-mode :bindings
      "?" 'docker-image-help
      "D" 'docker-image-rm
      "F" 'docker-image-pull
      "I" 'docker-image-inspect
      "P" 'docker-image-push
      "R" 'docker-image-run
      "T" 'docker-image-tag-selection
      "d" 'docker-image-mark-dangling
      "l" 'docker-image-ls)

    (evilified-state-evilify-map docker-container-mode-map :mode docker-container-mode :bindings
       "?" 'docker-container-help
       "C" 'docker-container-cp
       "D" 'docker-container-rm
       "I" 'docker-container-inspect
       "K" 'docker-container-kill
       "L" 'docker-container-logs
       "O" 'docker-container-stop
       "P" 'docker-container-pause
       "R" 'docker-container-restart
       "S" 'docker-container-start
       "a" 'docker-container-attach
       "b" 'docker-container-shells
       "d" 'docker-container-diff
       "f" 'docker-container-open
       "l" 'docker-container-ls
       "r" 'docker-container-rename-selection)

    (evilified-state-evilify-map docker-network-mode-map :mode docker-network-mode :bindings
       "?" 'docker-network-help
       "D" 'docker-network-rm
       "I" 'docker-network-inspect
       "d" 'docker-network-mark-dangling
       "l" 'docker-network-ls)

    (evilified-state-evilify-map docker-volume-mode-map :mode docker-volume-mode :bindings
       "?" 'docker-volume-help
       "D" 'docker-volume-rm
       "I" 'docker-volume-inspect
       "d" 'docker-volume-mark-dangling
       "f" 'docker-volume-dired-selection
       "l" 'docker-volume-ls)

    (evil-collection-define-key 'evilified 'tablist-minor-mode-map
      (kbd "TAB") 'tablist-forward-column
      [backtab] 'tablist-backward-column
      (kbd "]") 'tablist-forward-column
      (kbd "[") 'tablist-backward-column)))

;; (with-eval-after-load 'docker-core

;;   (defun docker-context-current ()
;;     (format "Context (%s)" (string-trim-right (shell-command-to-string "docker context show"))))

;;   (defun docker-context-list ()
;;     (split-string (string-trim-right (shell-command-to-string "docker context ls -q"))))

;;   (defun docker-context-use (context)
;;     (shell-command-to-string (format "docker context use %s" context))
;;     (message "Context set to %s" context))

;;   (defun docker-context ()
;;     (interactive)
;;     (helm :prompt "Docker Contexts: "
;;           :buffer "*helm-docker-contexts*"
;;           :sources
;;           (list (helm-build-sync-source "Docker Contexts"
;;                   :fuzzy-match  t
;;                   :candidates 'docker-context-list
;;                   :action 'docker-context-use))))

;;   (transient-define-prefix docker (arg)
;;     "Transient for docker."
;;     :man-page "docker"
;;     ["Arguments"
;;      (5 "H" "Host" "--host " read-string)
;;      (5 "Tt" "TLS" "--tls")
;;      (5 "Tv" "TLS verify remote" "--tlsverify")
;;      (5 "Ta" "TLS CA" "--tlscacert" docker-read-certificate)
;;      (5 "Tc" "TLS certificate" "--tlscert" docker-read-certificate)
;;      (5 "Tk" "TLS key" "--tlskey" docker-read-certificate)
;;      (5 "l" "Log level" "--log-level " docker-read-log-level)]
;;     ["Docker"
;;      ("c" (lambda ()(plist-get docker-status-strings :containers)) docker-containers)
;;      ("i" (lambda ()(plist-get docker-status-strings :images))     docker-images)
;;      ("n" (lambda ()(plist-get docker-status-strings :networks))   docker-networks)
;;      ("v" (lambda ()(plist-get docker-status-strings :volumes))    docker-volumes)]
;;     ["Other"
;;      ("x" docker-context-current docker-context)
;;      ("C" "Compose" docker-compose)]
;;     (interactive "P")
;;     (if arg
;;         (setq docker-command "docker")
;;       (if (derived-mode-p 'ssh-zsh-vterm-mode)
;;           (let* ((ssh-host (plist-get ssh-zsh-vterm-ssh-options :host)))
;;             (if ssh-host
;;                 (setq docker-command (format "ssh %s docker" ssh-host))))))
;;     (run-hooks 'docker-open-hook)
;;     (transient-setup 'docker)))

(with-eval-after-load 'docker-process
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

  (defun docker-run-async-with-buffer (program &rest args)
    "Execute \"PROGRAM ARGS\" and display output in a new buffer."
    (let ((default-directory (if (string-prefix-p "/scp:" default-directory)
                                 "~"
                               default-directory)))
      (apply docker-run-async-with-buffer-function program args))))

(with-eval-after-load 'docker-container
  (defun docker-container-vterm (container)
    "Open `vterm' in CONTAINER."
    (interactive (list (docker-container-read-name)))
    (require 'vterm nil 'noerror)
    (if (fboundp 'vterm-other-window)
        (let* ((container-address (format "docker:%s:/" container))
               (file-prefix (let ((ssh-host (seq--elt-safe (s-split " " docker-command) 1)))
                              (if ssh-host
                                  (format "/ssh:%s|" (s-chop-suffix ":" ssh-host))
                                "/")))
               (default-directory (format "%s%s" file-prefix container-address)))
          (zsh-vterm (docker-utils-generate-new-buffer-name "docker" "vterm:" default-directory)))
      (error "The vterm package is not installed"))))


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
        (docker-run-docker-async-with-buffer "container" "run" run-args it command)))))
