(require 'run-in-ghostel)

(define-derived-mode visidata-mode fundamental-mode "Visidata"
  (let* ((file (buffer-file-name))
         (cmd (format "visidata %s" file))
         (buf-name (format "*visidata %s*" file)))
    (kill-buffer)
    ;; Local visidata binary: pin locally (it cannot open TRAMP paths).
    (lx/run-in-ghostel cmd buf-name user-home-directory t)))

(provide 'visidata-mode)
