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


(defvar ghostel-mitmproxy-local-proxy-history nil)

(defun ghostel-mitmproxy-local-proxy (arg)
  (interactive "P")
  (let ((default-directory "~"))
    (let* ((local-spec (read-string "mitmproxy local mode spec: " nil
                                    '(ghostel-mitmproxy-local-proxy-history . 1)))
           (cmd (format "mitmproxy --showhost -k --mode local:%s" local-spec))
           (buffer-name "*mitmproxy*"))
      (lx/run-in-ghostel cmd buffer-name nil t))))

(provide 'ghostel-mitmproxy)
