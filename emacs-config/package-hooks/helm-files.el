;; -*- lexical-binding: t; -*-

;;; Helm action-buffer F-keys (C-z → *helm action*)
;;
;; C-z runs `helm-select-action', which shows `*helm action*'.  Labels
;; [f1]..[f12] are the 1st..12th entries of the current source's action
;; alist; F1..F12 call `helm-select-nth-action' with those indices.
;; Customizing those shortcuts means editing the action alist (not the
;; keymap).  File helms already rebind RET to
;; `spacemacs/helm-find-files-windows', so reordering the front of the
;; action list does not change RET open-file behavior.
;;
;; Desired front of file action lists:
;;   [f1] Find file in Dired
;;   [f2] Copy file path
;;   [f3] Copy file name

(defun lx/helm-copy-file-path (_candidate)
  "Copy absolute path(s) of the Helm selection / marked files.
Mirrors `spacemacs/copy-file-path' for Helm file candidates."
  (let* ((files (helm-marked-candidates))
         (paths (mapcar #'file-truename files))
         (joined (mapconcat #'identity paths "\n")))
    (kill-new joined)
    (message "%s" joined)))

(defun lx/helm-copy-file-name (_candidate)
  "Copy basename(s) of the Helm selection / marked files.
Mirrors `spacemacs/copy-file-name' for Helm file candidates."
  (let* ((files (helm-marked-candidates))
         (names (mapcar #'file-name-nondirectory files))
         (joined (mapconcat #'identity names "\n")))
    (kill-new joined)
    (message "%s" joined)))

(defun lx/helm-insert-copy-file-actions (actions)
  "Put Dired / copy-path / copy-name at [f1]/[f2]/[f3] in ACTIONS.
Idempotent on reload."
  (let* ((actions (if (and (symbolp actions) (boundp actions))
                      (symbol-value actions)
                    actions))
         (actions (cl-remove-if
                   (lambda (a)
                     (memq (cdr a) '(lx/helm-copy-file-path
                                     lx/helm-copy-file-name)))
                   actions))
         (dired-action (or (rassq 'helm-point-file-in-dired actions)
                           (rassq 'helm-open-dired actions)))
         (actions (if dired-action
                      (remove dired-action actions)
                    actions))
         (dired-action (or dired-action
                           '("Find file in Dired" . helm-point-file-in-dired)))
         (copy-path '("Copy file path" . lx/helm-copy-file-path))
         (copy-name '("Copy file name" . lx/helm-copy-file-name)))
    (append (list dired-action copy-path copy-name) actions)))

(with-eval-after-load 'helm-files
  ;; Initial listing newest-first; filtering still re-sorts by match score.
  (setq helm-ff-initial-sort-method 'newest)

  (setq helm-find-files-actions
        (lx/helm-insert-copy-file-actions helm-find-files-actions))
  (setq helm-type-file-actions
        (lx/helm-insert-copy-file-actions helm-type-file-actions))

  (with-eval-after-load 'helm-projectile
    (setq helm-projectile-file-actions
          (lx/helm-insert-copy-file-actions helm-projectile-file-actions)))

  ;; helm-list-dir-external needs GNU ls (-Q etc.); rpc→macOS/BSD ls fails
  ;; silently (exit 0, empty out) and recent helm no longer falls back to lisp.
  (defun lx/helm-list-directory-rpc-lisp (orig-fun directory &optional sel)
    (if (equal (file-remote-p directory 'method) "rpc")
        (helm-list-dir-lisp directory)
      (funcall orig-fun directory sel)))
  (advice-add 'helm-list-directory :around #'lx/helm-list-directory-rpc-lisp)

  (defun helm-substitute-in-filename (fname)
    (cond ((and ffap-url-regexp
                (string-match-p ffap-url-regexp fname))
           fname)
          ((and (file-remote-p fname)
                helm-substitute-in-filename-stay-on-remote)
           (let ((sub (substitute-in-file-name fname)))
             (if (file-directory-p sub)
                 sub (replace-regexp-in-string "/\\'" "" sub))))
          (t
           (with-temp-buffer
             (insert fname)
             (goto-char (point-min))
             (skip-chars-forward "/") ;; Avoid infloop in UNC paths Issue #424
             (if (re-search-forward "~/\\|//\\|/[[:alpha:]]:/" nil t)
                 (let ((match (match-string 0)))
                   (goto-char (if (or (string= match "//")
                                      (string-match-p "/[[:alpha:]]:/" match))
                                  (1+ (match-beginning 0))
                                (match-beginning 0)))
                   (buffer-substring-no-properties (point) (point-at-eol)))
               fname)))))

  (defvar autojump-db-file (if (lx/system-is-mac)
                               "~/Library/autojump/autojump.txt"
                             "~/.local/share/autojump/autojump.txt"))

  (defvar helm-autojump-command
    (format "sort -n -r %s | cut -f2 | sed 's/$/\\//'" autojump-db-file))

  (defun helm-autojump-candidates ()
    (--map (cons it it)
           (s-split "\n" (shell-command-to-string helm-autojump-command))))

  (defvar helm-autojump-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map helm-find-files-map)
                              (define-key map (kbd "RET") #'(lambda () (interactive)
                                                                (helm-run-after-quit #'(lambda (sel)
                                                                                         (helm-find-files-1 sel)) (helm-get-selection))))
                              map))

  (defclass helm-autojump-source (helm-source-sync)
    ((candidates :initform 'helm-autojump-candidates)
     (action :initform 'find-file)))

  (defvar helm-source-autojump (helm-make-source "Autojump" 'helm-autojump-source))

  (defun helm-autojump ()
    "Jump to a directory using autojump."
    (interactive)
    (helm :sources 'helm-source-autojump
          :buffer "*helm autojump*"
          :ff-transformer-show-only-basename nil))

  (spacemacs/set-leader-keys "oj" #'helm-autojump))
