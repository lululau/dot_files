(with-eval-after-load 'spaceline-segments
  (spaceline-define-segment persp-name
    "The current perspective name."
    (when (and t
               (bound-and-true-p persp-mode)
               ;; There are multiple implementations of
               ;; persp-mode with different APIs
               (fboundp 'safe-persp-name)
               (fboundp 'get-frame-persp)
               ;; Display the nil persp only if specified
               (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
                   spaceline-display-default-perspective))
      (let ((name (safe-persp-name (get-frame-persp))))
        (propertize
         (if (file-directory-p name)
             (file-name-nondirectory (directory-file-name name))
           name)
         'face 'bold)))))
