(require 'run-in-ghostel)

(defvar ghostel-mitmproxy-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun ghostel-mitmproxy-transparent-proxy ()
  (interactive)
  (let ((default-directory "~"))
    (let* ((cmd (format "%s/start_transparent_mitmproxy.sh" ghostel-mitmproxy-dir))
           (buffer-name "*mitmproxy*"))
      (lx/run-in-ghostel cmd buffer-name nil t))))

(defun ghostel-mitmproxy-normal-proxy (arg)
  (interactive "P")
  (let ((default-directory "~"))
    (let* ((cmd (format "%s/start_normal_mitmproxy.sh %s" ghostel-mitmproxy-dir (if arg "true" "false")))
           (buffer-name "*mitmproxy*"))
      (lx/run-in-ghostel cmd buffer-name nil t))))

(provide 'ghostel-mitmproxy)
