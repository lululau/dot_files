;; -*- lexical-binding: t; -*-

;; Recognize PEP 723 inline-script shebangs (`uv run --script`, `uvx`) as Python.
;; `set-auto-mode' extracts the interpreter name from the shebang line, but by
;; default `interpreter-mode-alist' only knows `python[0-9.]*'. Scripts that use
;; `#!/usr/bin/env -S uv run --script` resolve to interpreter `uv' and otherwise
;; fall back to fundamental-mode.
(dolist (interp '("uv" "uvx"))
  (add-to-list 'interpreter-mode-alist (cons interp 'python-mode)))

(spacemacs|use-package-add-hook python
  :post-config
  (defun python-shell-send-line (&optional send-main)
    "Send the entire buffer to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== '__main__':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument."
    (interactive "P")
    (save-restriction
      (widen)
      (python-shell-send-region (line-beginning-position) (line-end-position) send-main)))

  (defun python-shell-send-line-switch ()
    (interactive)
    (python-shell-send-line)
    (python-shell-switch-to-shell)
    (evil-insert-state))

  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "sL" 'python-shell-send-line-switch
    "sl" 'python-shell-send-line)
  (define-key python-mode-map (kbd "C-M-i") nil)
  )

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "s-r c") #'spacemacs/python-start-or-switch-repl))
