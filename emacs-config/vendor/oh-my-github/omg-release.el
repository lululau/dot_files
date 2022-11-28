;;; -*- lexical-binding: t -*-

(require 'omg-dyn)
(require 'tabulated-list)
(require 'seq)

(defcustom omg-release-query-limit 50
  "Limit used when query latest releases (max 100)."
  :group 'omg
  :type 'integer)

(defun omg-release--query ()
  (seq-into (omg-dyn-query-releases omg-repo--current-full-name
                                    omg-release-query-limit)
            'list))

(defun omg-release--get-name ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 1)))

(defun omg-release--get-tag ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 3)))

(defun omg-release--get-url ()
  (when-let ((tag (omg-release--get-tag)))
    (format "https://github.com/%s/releases/%s"
            omg-repo--current-full-name
            tag)))

(defun omg-release-browse ()
  "Browse release at point."
  (interactive)
  (if-let* ((url (omg-release--get-url)))
      (browse-url url)
    (user-error "There is no release at point")))

(defun omg-release-copy-url ()
  "Browse release at point."
  (interactive)
  (if-let* ((url (omg-release--get-url)))
      (progn
        (kill-new url)
        (message "Copied %s" url))
    (user-error "There is no release at point")))

(defvar omg-release-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "b") 'omg-release-browse)
    (define-key map (kbd "w") 'omg-release-copy-url)
    (define-key map (kbd "RET") 'omg-release-query-assets)
    map)
  "Local keymap for omg-release-mode buffers.")

(define-derived-mode omg-release-mode tabulated-list-mode "omg-release" "Display releases of GitHub repository"
  (setq tabulated-list-format [("PublishedAt" 20 t)
                               ("Name" 60)
                               ("Author" 20 t)
                               ("Tag" 15 t)
                               ("Draft" 6)
                               ("Prerelease" 6)]

        tabulated-list-padding 2
        tabulated-list-sort-key (cons "PublishedAt" t)
        tabulated-list-entries 'omg-release--query)
  (tabulated-list-init-header))

(defun omg-release-query-assets ()
  (interactive)
  (when-let* ((name (omg-release--get-name))
              (label (car name))
              (files (plist-get (cdr name) 'asset-files)))
    (with-current-buffer (get-buffer-create (format "*omg-release %s assets*" label))
      (omg-release-asset-mode)
      (setq tabulated-list-entries (seq-into files 'list))
      (tabulated-list-print t)
      (switch-to-buffer (current-buffer)))))

(defun omg-release--get-asset-name ()
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

(defun omg-release-copy-asset-url ()
  (interactive)
  (if-let* ((name (omg-release--get-asset-name))
            (raw-url (plist-get (cdr name) 'raw-url)))
      (progn
        (kill-new raw-url)
        (message "Copied %s" raw-url))
    (user-error "There is no asset at point")))

(defun omg-release-download-asset ()
  "Browse release at point."
  (interactive)
  (if-let* ((name (omg-release--get-asset-name))
            (label (car name))
            (raw-url (plist-get (cdr name) 'raw-url)))
      (omg--download-file label raw-url)
    (user-error "There is no asset at point")))

(defvar omg-release-asset-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "w") 'omg-release-copy-asset-url)
    (define-key map (kbd "RET") 'omg-release-download-asset)
    map)
  "Local keymap for omg-release-asset-mode buffers.")

(define-derived-mode omg-release-asset-mode tabulated-list-mode "omg-release assets" "Display Assets of GitHub repository"
  (setq tabulated-list-format [("Name" 40 t)
                               ("Size" 8)
                               ("Download Count" 5 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" t))
  (tabulated-list-init-header))

(provide 'omg-release)

;; Local Variables:
;; coding: utf-8
;; End:

;;; omg-release.el ends here
