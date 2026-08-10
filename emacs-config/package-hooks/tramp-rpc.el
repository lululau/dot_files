;; -*- lexical-binding: t; -*-

(with-eval-after-load 'tramp-rpc-deploy
  ;; Fix tramp-rpc-deploy--download-file header separator parsing bug
  (defun tramp-rpc-deploy--download-file (url dest)
    "Download URL to DEST synchronously.
Returns t on success, nil on failure."
    (condition-case err
        (let ((url-request-method "GET")
              (url-show-status nil))
          (message "Downloading %s..." url)
          (with-timeout (tramp-rpc-deploy-download-timeout
                         (signal 'remote-file-error
                                 (list (format "Download timed out after %d seconds"
                                               tramp-rpc-deploy-download-timeout))))
            (let ((buffer (url-retrieve-synchronously url t t)))
              (unless buffer
                (signal 'remote-file-error (list "No HTTP response" url)))
              (unwind-protect
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (unless (looking-at "HTTP/[0-9.]+ 200\\(?:[ \t]\\|$\\)")
                      (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                          (signal 'remote-file-error (list "HTTP error" (match-string 1)))
                        (signal 'remote-file-error (list "Invalid HTTP response"))))
                    (if (re-search-forward "\r?\n\r?\n" nil t)
                        (point)
                      (when (boundp 'url-http-end-of-headers)
                        (goto-char url-http-end-of-headers)
                        (while (memq (char-after) '(13 10))
                          (forward-char 1))))
                    (let ((coding-system-for-write 'binary))
                      (write-region (point) (point-max) dest nil 'silent))
                    t)
                (when (buffer-live-p buffer)
                  (kill-buffer buffer))))))
      (error
       (message "Download failed: %s" (error-message-string err))
       nil))))
