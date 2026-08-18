;; -*- lexical-binding: t; -*-

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<M-return>") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "<S-return>") 'lsp-find-references))

(with-eval-after-load 'lsp-icons
  (defcustom lx/lsp-headerline-icon-size 16
    "Size (width and height in pixels) for lsp-headerline breadcrumb icons."
    :type 'integer
    :group 'lsp-icons)

  (defcustom lx/lsp-headerline-icon-margin '(2 . 1)
    "Margin (padding) for lsp-headerline breadcrumb icons.
Can be an integer or a cons cell (X . Y) specifying horizontal and vertical margins."
    :type '(choice integer (cons integer integer))
    :group 'lsp-icons)

  (defun lx/lsp-icons--fix-image-background-advice (orig-fn image)
    "Adjust size and margin for lsp-headerline breadcrumb icons."
    (if image
        (let ((display-image (get-text-property 0 'display image)))
          (if (and (listp display-image)
                   (plist-member (cl-copy-list (cl-rest display-image)) :type))
              (let ((props (cl-copy-list (cl-rest display-image))))
                (setq props (plist-put props :background (face-attribute 'header-line :background nil t)))
                (when lx/lsp-headerline-icon-size
                  (setq props (plist-put props :width lx/lsp-headerline-icon-size))
                  (setq props (plist-put props :height lx/lsp-headerline-icon-size)))
                (when lx/lsp-headerline-icon-margin
                  (setq props (plist-put props :margin lx/lsp-headerline-icon-margin)))
                (propertize " " 'display (cl-list* 'image props)))
            (funcall orig-fn image)))
      ""))

  (advice-add 'lsp-icons--fix-image-background :around #'lx/lsp-icons--fix-image-background-advice))

