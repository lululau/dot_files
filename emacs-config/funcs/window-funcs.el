(defun lx/delete-window-direction (direction)
  "Delete window in DIRECTION"
  (let ((wind (windmove-find-other-window direction)))
    (when (and wind (not (minibufferp (window-buffer wind))))
      (delete-window wind))))

;;;###autoload
(defun lx/delete-window-below ()
  "Delete window below"
  (interactive)
  (lx/delete-window-direction 'down))

;;;###autoload
(defun lx/delete-window-above ()
  "Delete window above"
  (interactive)
  (lx/delete-window-direction 'up))

;;;###autoload
(defun lx/delete-window-left ()
  "Delete window left"
  (interactive)
  (lx/delete-window-direction 'left))

;;;###autoload
(defun lx/delete-window-right ()
  "Delete window right"
  (interactive)
  (lx/delete-window-direction 'right))

;;;###autoload
(defun lx/popwin-rspec-buffer ()
  "Pop to rspec compilation buffer via popwin"
  (interactive)
  (popwin:pop-to-buffer (get-buffer "*rspec-compilation*"))
  (delete-window (get-buffer-window " *popwin-dummy*"))
  (select-window (get-buffer-window "*rspec-compilation*")))
