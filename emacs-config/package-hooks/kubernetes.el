;; (defun kubernetes-kubectl (props state args on-success &optional on-error cleanup-cb)
;;   (let* ((buf (generate-new-buffer " kubectl"))
;;          (err-buf (generate-new-buffer " kubectl-err"))
;;          (subcommand (first args))
;;          (flags (kubernetes-kubectl--flags-from-state state))
;;          (flags (if (-contains? '("config" "exec" "describe") subcommand) (-remove-item "--all-namespaces" flags) flags))
;;          (command (append (list kubernetes-kubectl-executable) args flags))
;;          ;; `default-directory' must exist, otherwise `make-process' raises an
;;          ;; error.
;;          (default-directory (kubernetes-utils-up-to-existing-dir default-directory))
;;          (proc (make-process
;;                 :name "kubectl"
;;                 :buffer buf
;;                 :stderr err-buf
;;                 :command command
;;                 :noquery t
;;                 :sentinel
;;                 (lambda (proc status)
;;                   (unwind-protect
;;                       (let ((exit-code (process-exit-status proc)))
;;                         (cond
;;                          ((zerop exit-code)
;;                           (funcall on-success buf))
;;                          (t
;;                           (let ((err-message (with-current-buffer err-buf (buffer-string))))
;;                             (unless (= 9 exit-code)
;;                               (kubernetes-props-update-last-error props err-message (string-join command " ") (current-time))))
;;                           (cond (on-error
;;                                  (funcall on-error err-buf))
;;                                 (t
;;                                  (kubernetes-kubectl--default-error-handler props status))))))
;;                     (when cleanup-cb
;;                       (funcall cleanup-cb))
;;                     (kubernetes-process-kill-quietly proc))))))
;;     ;; Clean up stderr buffer when stdout buffer is killed.
;;     (with-current-buffer buf
;;       (add-hook 'kill-buffer-hook (lambda ()
;;                                     (let ((kill-buffer-query-functions nil))
;;                                       (ignore-errors (kill-buffer err-buf))))
;;                 nil t))
;;     proc))

(with-eval-after-load 'kubernetes-commands
  (defun kubernetes--namespace-names (state)
    (-let* ((config (or (kubernetes-state-namespaces state) (kubernetes-kubectl-await-on-async kubernetes-props state #'kubernetes-kubectl-get-namespaces)))
            ((&alist 'items items) config))
      (cons "all" (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))))

(with-eval-after-load 'kubernetes-kubectl
  (defun kubernetes-kubectl--flags-from-state (state)
    (append (when-let (ns (kubernetes-state-current-namespace state))
              (if (string= ns "all")
                  '("--all-namespaces")
                (list (format "--namespace=%s" ns))))
            (kubernetes-state-kubectl-flags state))))
