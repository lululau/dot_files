;; -*- lexical-binding: t; -*-

(with-eval-after-load 'helm-files
  ;; Initial listing newest-first; filtering still re-sorts by match score.
  (setq helm-ff-initial-sort-method 'newest)

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
