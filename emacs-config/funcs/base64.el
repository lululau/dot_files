(defun base64-decode-utf8-region (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (base64-decode-region (point-min) (point-max))
    (decode-coding-region (point-min) (point-max) 'utf-8)))

(defun base64-encode-utf8-region (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (encode-coding-region (point-min) (point-max) 'utf-8)
    (base64-encode-region (point-min) (point-max) t)))
