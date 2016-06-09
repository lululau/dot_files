(spacemacs|use-package-add-hook yaml-mode
  :post-config
  (defun lx/yaml-leading-spaces (line)
    (length (car (s-match "^ *" line))))

  (defun lx/yaml-get-key (line)
    (s-trim (car (s-split ":" line))))

  (defun lx/yaml-get-path ()
    (save-excursion
      (let (line leading-spaces result)
        (s-join "."
                (catch 'while
                  (while t
                    (setq line (thing-at-point 'line t))
                    (when (or (null leading-spaces) (< (lx/yaml-leading-spaces line) leading-spaces))
                      (setq leading-spaces (lx/yaml-leading-spaces line))
                      (push (lx/yaml-get-key line) result))
                    (if (= (line-number-at-pos) 1) (throw 'while result))
                    (forward-line -1)))))))

  (defun lx/yaml-show-and-copy-path ()
    (interactive)
    (let ((path (lx/yaml-get-path)))
      (kill-new path)
      (message path)))

  (spacemacs/set-leader-keys-for-major-mode 'yaml-mode "s" #'lx/yaml-show-and-copy-path))
