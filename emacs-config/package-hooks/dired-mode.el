(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "n") nil)
  (define-key dired-mode-map (kbd "g") nil)
  (define-key dired-mode-map (kbd "G") nil)
  (define-key dired-mode-map (kbd "?") nil)
  (define-key dired-mode-map (kbd ")") 'dired-up-directory)
  (define-key dired-mode-map (kbd "-") 'dired-up-directory)
  (define-key dired-mode-map (kbd "S-SPC") nil)
  (define-key dired-mode-map (kbd "TAB") 'dired-subtree-cycle)
  (unless (or (display-graphic-p) (lx/system-is-linux))
    (defun dired-delete-file (file &optional recursive trash)
      (call-process "trash" nil nil nil file)))

  (defun dired-dotfiles-toggle ()
    "Show/hide dot-files"
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (message "h")
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        (progn (revert-buffer) ; otherwise just revert to re-show
               (set (make-local-variable 'dired-dotfiles-show-p) t))))))

(with-eval-after-load 'dired-x
  (define-key dired-mode-map (kbd "N") nil))
