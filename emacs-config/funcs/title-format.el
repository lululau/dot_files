(defun lx/layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ". " string-name)))
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
                             persp-list spaces)))
         (file (if (projectile-project-p)
                    (if (buffer-file-name)
                        (s-replace (projectile-project-root) (format "【%s】" (projectile-project-name)) (buffer-file-name))
                      (buffer-name))
                 (if (buffer-file-name)
                     (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
                         (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
                       (buffer-file-name)) (buffer-name)))))
    (concat file "     -     " formatted-persp-list)))

(defun lx/default-title-bar ()
  (if (projectile-project-p)
      (concat
       (projectile-project-name)
       (if (buffer-file-name)
           (concat "  ✈  " (substring (buffer-file-name) (length (projectile-project-root))))
         (concat "  ✈  "(buffer-name))))
    (if (buffer-file-name)
        (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
            (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
          (buffer-file-name)) (buffer-name))))

(defun lx/toggle-title-format()
  (interactive)
  (if (equal frame-title-format '(:eval (lx/layouts-for-title-bar)))
      (setq frame-title-format '(:eval (lx/default-title-bar)))
    (setq frame-title-format '(:eval (lx/layouts-for-title-bar))))
  (redraw-frame))

