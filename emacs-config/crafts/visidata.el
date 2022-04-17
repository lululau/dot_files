(require 'run-in-vterm)

(define-derived-mode visidata-mode fundamental-mode "Visidata"
  (let* ((file (buffer-file-name))
         (cmd (format "visidata %s" file))
         (buf-name (format "*visidata %s*" file)))
    (kill-buffer)
    (lx/run-in-vterm cmd buf-name nil t)))

(provide 'visidata-mode)
