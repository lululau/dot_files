(with-eval-after-load 'hybrid-mode
  (defun hybrid-mode//update-states-for-current-buffers (style)
    "Update the active state in all current buffers given current STYLE."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p evil-local-mode)
                   (not (minibufferp)))
          (cond
           ((eq 'hybrid style)
            (if (memq major-mode evil-evilified-state-modes)
                (evil-evilified-state)
              (funcall (intern (format "evil-%S-state"
                                       hybrid-style-default-state)))))
           ((and (eq 'vim style)
                 (memq evil-state '(hybrid emacs)))
            (cond
             ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
             ((memq major-mode evil-motion-state-modes) (evil-motion-state))
             (t (evil-normal-state))))))))))
