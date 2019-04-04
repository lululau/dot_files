(cl-defun helm-mm-pinyin-match (str &optional (pattern helm-pattern))
  (string-match-p (pinyinlib-build-regexp-string pattern) str))
