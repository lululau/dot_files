(require 'vterm)

(defun lx/run-in-pry-vterm (command buffer-name &optional directory exclusive-window)
  (interactive)
  (let* ((buffer (get-buffer buffer-name)))
    (set (intern (format "%s-command" buffer-name)) (list command buffer-name directory exclusive-window))
    (set (intern (format "%s-process-environment" buffer-name)) process-environment)
    (set (intern (format "%s-kill-buffer-on-exit" buffer-name)) (bound-and-true-p vterm-kill-buffer-on-exit))
    (if buffer
        (if (equal buffer (current-buffer))
            (if (and (eq 1 (length (window-list))) (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (if exclusive-window
              (switch-to-buffer buffer)
            (pop-to-buffer buffer 'display-buffer-pop-up-window)))
      (let* ((default-directory (or directory user-home-directory))
             (vterm-shell command))
        (unless exclusive-window (split-window-right-and-focus))
        (pry-vterm buffer-name)))))

(defun pry-vterm (&optional arg)
  "Create an interactive Vterm buffer.
Start a new Vterm session, or switch to an already active
session.  Return the buffer selected (or created).

With a nonnumeric prefix arg, create a new session.

With a string prefix arg, create a new session with arg as buffer name.

With a numeric prefix arg (as in `C-u 42 M-x vterm RET'), switch
to the session with that number, or create it if it doesn't
already exist.

The buffer name used for Vterm sessions is determined by the
value of `vterm-buffer-name'."
  (interactive "P")
  (pry-vterm--internal #'pop-to-buffer-same-window arg))

(defun pry-vterm--internal (pop-to-buf-fun &optional arg)
  (cl-assert vterm-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               vterm-buffer-name
                                               arg)))
                   ((stringp arg) (generate-new-buffer arg))
                   (arg (generate-new-buffer vterm-buffer-name))
                   (t
                    (get-buffer-create vterm-buffer-name)))))
    (cl-assert (and buf (buffer-live-p buf)))
    (funcall pop-to-buf-fun buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'pry-vterm-mode)
        (pry-vterm-mode)))
    buf))

(defun pry-vterm-get-current-line ()
  (let* ((start (line-beginning-position))
        (end (point))
        (str (buffer-substring-no-properties start end)))
    (replace-regexp-in-string "^.*[❯>] ?" "" str)))

(defun pry-vterm-accept-copilot-or-send-tab-to-term ()
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion)
    (vterm-send-tab)))

(defvar pry-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map vterm-mode-map)
    (define-key map (kbd "<backtab>") #'copilot-accept-completion)
    (define-key map (kbd "<tab>") #'pry-vterm-accept-copilot-or-send-tab-to-term)
    map))

(define-derived-mode pry-vterm-mode vterm-mode "Pry"
  "Major mode for pry vterm buffer.")

(setplist 'pry-vterm-mode (plist-put (symbol-plist 'pry-vterm-mode) 'insert-function 'vterm-send-string))

(provide 'pry-vterm)
