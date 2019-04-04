(cl-defun helm-mm-pinyin-match (str &optional (pattern helm-pattern))
  (string-match-p (s-join ".*" (mapcar (lambda (char)  (pinyinlib-build-regexp-char char)) pattern)) str))
