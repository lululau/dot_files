(require 'run-in-vterm)

(defvar vterm-mitmproxy-dir (file-name-directory load-file-name))

(defun vterm-mitmproxy-transparent-proxy ()
  (interactive)
  (let* ((cmd (format "%s/start_transparent_mitmproxy.sh" vterm-mitmproxy-dir))
         (buffer-name "*mitmproxy*"))
    (lx/run-in-vterm cmd buffer-name nil t)))

(defun vterm-mitmproxy-normal-proxy (arg)
  (interactive "P")
  (let* ((cmd (format "%s/start_normal_mitmproxy.sh %s" vterm-mitmproxy-dir (if arg "true" "false")))
         (buffer-name "*mitmproxy*"))
    (lx/run-in-vterm cmd buffer-name nil t)))

(provide 'vterm-mitmproxy)
