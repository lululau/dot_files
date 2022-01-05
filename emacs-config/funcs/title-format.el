(defun lx/layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (car (last (split-string name "/" t))))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ". " layout-name)))
    (if current
        ;; (propertize (concat "❰❰ " caption " ❱❱") 'face 'warning)
        (propertize (concat "★ " caption) 'face 'warning)
      caption)))


(defun lx/layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (lx/layout-format-name
                                persp (position persp persp-list)))
                             persp-list spaces))))
    formatted-persp-list))

(defun lx/default-title-bar ()
  (format "%s     -      %s"
          (car (last (split-string (or (spacemacs//current-layout-name) "") "/" t)))
          (or (buffer-file-name) (buffer-name))))

(defun lx/toggle-title-format()
  (interactive)
  (if (equal frame-title-format '(:eval (lx/layouts-for-title-bar)))
      (setq frame-title-format '(:eval (lx/default-title-bar)))
    (setq frame-title-format '(:eval (lx/layouts-for-title-bar))))
  (redraw-frame))

