(spacemacs/declare-prefix "o" "user key bindings")
(spacemacs/declare-prefix "oo" "open org files")
(spacemacs/declare-prefix "oi" "open info nodes")

(defun lx/open-bookmarks.org ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/bookmarks.org"))

(defun lx/open-notes.org ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/notes.org"))

(defun lx/open-demo.http ()
  (interactive)
  (find-file "~/Library/Mobile Documents/com~apple~CloudDocs/org/demo.http"))

(defun lx/open-emacs-info ()
  (interactive)
  (org-open-link-from-string "info:emacs#Top"))

(defun lx/open-elisp-info ()
  (interactive)
  (org-open-link-from-string "info:elisp#Top"))

(defun lx/open-org-info ()
  (interactive)
  (org-open-link-from-string "info:org#Top"))

(defun lx/open-mu4e-info ()
  (interactive)
  (org-open-link-from-string "info:mu4e#Top"))

(defun lx/open-magit-info ()
  (interactive)
  (org-open-link-from-string "info:magit#Top"))

(defun lx/open-evil-info ()
  (interactive)
  (org-open-link-from-string "info:evil#Top"))

(spacemacs/set-leader-keys

  ;; Org
  "oob" #'lx/open-bookmarks.org
  "oon" #'lx/open-notes.org
  "ooh" #'lx/open-demo.http

  ;; Info bookmarks
  "oie" #'lx/open-emacs-info
  "oil" #'lx/open-elisp-info
  "oio" #'lx/open-org-info
  "oim" #'lx/open-mu4e-info
  "oig" #'lx/open-magit-info
  "oiv" #'lx/open-evil-info)
