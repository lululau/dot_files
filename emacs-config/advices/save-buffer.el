(setq scratch-buffer-content nil)

(defun save-buffer-advice (origfunc &rest args)
  (let ((content (buffer-string)))
    (if (string= (buffer-name) "*scratch*")
        (if (string= content scratch-buffer-content)
            (message "No changes to save")
          (progn
            (setq scratch-buffer-content content)
            (lx/save-scratch)))
      (apply origfunc args))))

(advice-add 'save-buffer :around #'save-buffer-advice)
