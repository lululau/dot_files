;;;;;;;  1. Replace kubernetes-kubectl in kubernetes-kubectl.el
;;;;;;;  2. Move kubernetes-utils--save-window-state from kubernetes-utils.el to kubernetes-core.el


;; (cl-defun kubernetes-kubectl (props
;;                               state
;;                               args
;;                               on-success
;;                               &optional on-error cleanup-cb
;;                               &key flags)
;;   "Run kubectl with ARGS.

;; PROPS is an alist of functions to inject.  It should normally be passed
;; `kubernetes-props'.

;; STATE is the current application state, used to apply additional
;; global flags to kubectl.  If FLAGS is set, this is ignored and
;; can safely be set to nil.

;; ON-SUCCESS is a function of one argument, called with the process' buffer.

;; Optional ON-ERROR is a function of two arguments, called with the
;; process' stderr buffer.  If omitted, it defaults to
;; `kubernetes-kubectl--default-error-handler', which logs an error
;; if the process exited unexpectedly.

;; Optional CLEANUP-CB is a function of no arguments that is always
;; called after the other callbacks.  It can be used for releasing
;; resources.

;; After callbacks are executed, the process and its buffer will be killed.

;; Returns the process object for this execution of kubectl."
;;   (let* ((flags (or flags (kubernetes-kubectl--flags-from-state state)))
;;          (subcommand (first args))
;;          (flags (if (-contains? '("config" "exec" "describe") subcommand) (-remove-item "--all-namespaces" flags) flags))
;;          (command (append (list kubernetes-kubectl-executable) args flags))
;;          (buf (generate-new-buffer (format " kubectl: %s" command)))
;;          (err-buf (generate-new-buffer (format " kubectl-err: %s" command)))

;;          ;; `default-directory' must exist, otherwise `make-process' raises an
;;          ;; error.
;;          (default-directory (kubernetes-utils-up-to-existing-dir default-directory)))

;;     ;; Clean up stderr buffer when stdout buffer is killed.
;;     (with-current-buffer buf
;;       (add-hook 'kill-buffer-hook (lambda ()
;;                                     (let ((kill-buffer-query-functions nil))
;;                                       (ignore-errors (kill-buffer err-buf))))
;;                 nil t))

;;     (make-process
;;      :name (format "kubectl: %s" (s-join " " command))
;;      :buffer buf
;;      :stderr err-buf
;;      :command command
;;      :noquery t
;;      :sentinel
;;      (lambda (proc status)
;;        (unwind-protect
;;            (let ((exit-code (process-exit-status proc)))
;;              (cond
;;               ((zerop exit-code)
;;                (funcall on-success buf))
;;               (t
;;                (let ((err-message (with-current-buffer err-buf (buffer-string))))
;;                  (unless (= 9 exit-code)
;;                    (kubernetes-props-update-last-error props err-message (string-join command " ") (current-time))))
;;                (cond (on-error
;;                       (funcall on-error err-buf))
;;                      (t
;;                       (kubernetes-kubectl--default-error-handler props status))))))
;;          (when cleanup-cb
;;            (funcall cleanup-cb))
;;          (kubernetes-process-kill-quietly proc))))))

(with-eval-after-load 'kubernetes-commands
  (defun kubernetes--namespace-names (state)
    (-let* ((config (or (kubernetes-state--get state 'namespaces) (kubernetes-kubectl-await-on-async kubernetes-props state (-partial #'kubernetes-kubectl-get "namespaces"))))
            ((&alist 'items items) config))
      (cons "all" (-map (-lambda ((&alist 'metadata (&alist 'name name))) name) items)))))

(with-eval-after-load 'kubernetes-kubectl
  (defun kubernetes-kubectl--flags-from-state (state)
    (append (when-let (ns (kubernetes-state--get state 'current-namespace))
              (if (string= ns "all")
                  '("--all-namespaces")
                (list (format "--namespace=%s" ns))))
            (kubernetes-state-kubectl-flags state))))
