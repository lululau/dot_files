(with-eval-after-load 'helm-flx
  (cl-defun helm-flx-fuzzy-highlight-match (candidate
                                         &optional (pattern helm-pattern) diacritics file-comp)
    (require 'flx)
    (if (string-match-p " " helm-pattern)
        (helm-fuzzy-default-highlight-match candidate diacritics)
      (let* ((candidate (helm-flx-candidate-string candidate))
             (pair (and (consp candidate) candidate))
             (display (if pair (car pair) candidate))
             (real (cdr pair)))
        (setq display (helm-flx-fuzzy-highligher display helm-pattern))
        (if real (cons display real) display)))))
