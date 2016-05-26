(defun jump-to-definition-of-symbol-at-point ()
  (interactive)
  (if (bound-and-true-p robe-mode)
      (if (and (symbol-at-point) (zerop (call-process "bash" nil nil nil "-c" (concat "[ -z $(global --result=grep -i " (thing-at-point 'symbol) ") ]"))))
          (call-interactively 'robe-jump)
        (call-interactively 'helm-gtags-find-tag))
    (call-interactively 'helm-gtags-find-tag)))

