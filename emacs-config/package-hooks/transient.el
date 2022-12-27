(with-eval-after-load 'transient
  (transient-bind-q-to-quit)

  ;; docker.el require these transient functions from latest version of transient, which is newer t the one in emacs 28.2
  (cl-defgeneric transient-default-value (_)
    "Return the default value."
    nil)

  (cl-defmethod transient-default-value ((obj transient-prefix))
    (if-let ((default (and (slot-boundp obj 'default-value)
                           (oref obj default-value))))
        (if (functionp default)
            (funcall default)
          default)
      nil)))
