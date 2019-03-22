(defun lx/remove-empty-lines (arg)
  "Remove empty lines"
  (interactive "P")
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (if arg
            (while (re-search-forward "\\(^\\s-*\n\\)+" end t)
              (replace-match "\n"))
          (while (re-search-forward "^\\s-*\n" end t)
            (replace-match "")))))))

