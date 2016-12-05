(defun lx/window-move-very-top ()
  (interactive)
  (if (neo-global--window-exists-p)
      (progn
        (delete-window neo-global--window)
        (call-interactively 'evil-window-move-very-top)
        (let ((win (get-buffer-window)))
          (neo-global--open)
          (select-window win)))
    (call-interactively 'evil-window-move-very-top)))

(defun lx/window-move-very-bottom ()
  (interactive)
  (if (neo-global--window-exists-p)
      (progn
        (delete-window neo-global--window)
        (call-interactively 'evil-window-move-very-bottom)
        (let ((win (get-buffer-window)))
          (neo-global--open)
          (select-window win)))
    (call-interactively 'evil-window-move-very-bottom)))

(defun lx/window-move-far-left ()
  (interactive)
  (if (neo-global--window-exists-p)
      (progn
        (delete-window neo-global--window)
        (call-interactively 'evil-window-move-far-left)
        (let ((win (get-buffer-window)))
          (neo-global--open)
          (select-window win)))
    (call-interactively 'evil-window-move-far-left)))

(defun lx/window-move-far-right ()
  (interactive)
  (if (neo-global--window-exists-p)
      (progn
        (delete-window neo-global--window)
        (call-interactively 'evil-window-move-far-right)
        (let ((win (get-buffer-window)))
          (neo-global--open)
          (select-window win)))
    (call-interactively 'evil-window-move-far-right)))

(defmacro lx/def-window-frame-switch-function (direct-arg)
  (let ((direct (eval direct-arg)))
    `(defun ,(intern (format "lx/window-%s-fallback-to-switch-frame" direct)) ()
       (interactive)
       (condition-case
           err
           (call-interactively ',(intern (format "evil-window-%s" direct)))
         (user-error (let ((message  (error-message-string err)))
                       (if (and (= 2 (length (frame-list)))
                                (string-match "Minibuffer is inactive\\|No Window \\w+ from selected window" message))
                           (other-frame 1)
                         (signal (car err) (cdr err)))))))))

(lx/def-window-frame-switch-function 'up)
(lx/def-window-frame-switch-function 'down)
