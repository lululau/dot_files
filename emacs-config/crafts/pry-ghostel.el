(require 'ghostel)

(defun lx/run-in-pry-ghostel (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p ghostel-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (command-parts (split-string-and-unquote command))
             (buffer (generate-new-buffer buffer-name)))
        (unless exclusive-window (split-window-right-and-focus))
        (with-current-buffer buffer
          (pry-ghostel-mode))
        (pop-to-buffer buffer (append display-buffer--same-window-action
                                      '((category . comint))))
        (ghostel-exec buffer (car command-parts) (cdr command-parts))))))

(defun pry-ghostel (&optional arg)
  "Create an interactive Ghostel buffer.
Start a new Ghostel session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x ghostel RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Ghostel sessions is determined by the
value of `ghostel-buffer-name'."
  (interactive "P")
  (pry-ghostel--internal #'pop-to-buffer-same-window arg))

(defun pry-ghostel--internal (pop-to-buf-fun &optional arg)
  (cl-assert ghostel-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               ghostel-buffer-name
                                               arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer ghostel-buffer-name))
                   (t
                    (get-buffer-create ghostel-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (funcall pop-to-buf-fun buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'pry-ghostel-mode)
        (pry-ghostel-mode)))
    (ghostel--init-buffer buf (buffer-name buf))
    buf))

(defun pry-ghostel-get-current-line ()
  (let* ((start (line-beginning-position))
        (end (point))
        (str (buffer-substring-no-properties start end)))
    (replace-regexp-in-string "^.*[❯>] ?" "" str)))

(defun pry-ghostel-accept-copilot-or-send-tab-to-term ()
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (ghostel-send-key "tab")))

(defvar pry-ghostel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ghostel-semi-char-mode-map)
    (define-key map (kbd "<backtab>") #'copilot-accept-completion)
    (define-key map (kbd "<tab>") #'pry-ghostel-accept-copilot-or-send-tab-to-term)
    map))

(define-derived-mode pry-ghostel-mode ghostel-mode "Pry"
  "Major mode for pry ghostel buffer."
  (use-local-map pry-ghostel-mode-map))

(defun pry-ghostel--restore-keymap (&rest _)
  "Restore `pry-ghostel-mode-map' after ghostel switches back to semi-char mode."
  (when (derived-mode-p 'pry-ghostel-mode)
    (use-local-map pry-ghostel-mode-map)))

(advice-add 'ghostel-semi-char-mode :after #'pry-ghostel--restore-keymap)

(provide 'pry-ghostel)
