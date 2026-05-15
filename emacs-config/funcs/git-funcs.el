;;;###autoload
(defun lx/magit-status-smart (arg)
  "Call magit-status. With prefix arg, switch to existing magit buffer if any"
  (interactive "P")
  (if arg
      (progn
        (require 'magit-mode)
        (let ((magit-buf (--find (s-starts-with? "magit:" (buffer-name it)) (magit-mode-get-buffers))))
          (if magit-buf (switch-to-buffer magit-buf) (magit-status))))
    (magit-status)))
