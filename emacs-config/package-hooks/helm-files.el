(with-eval-after-load 'helm-files
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

  (defclass helm-autojump-source (helm-source-ffiles)
    ((candidates :initform 'helm-autojump-candidates)
     (fuzzy-match :initform t)
     (filtered-candidate-transformer :initform nil)
     (keymap :initform helm-autojump-map)))

  (defvar helm-source-autojump (helm-make-source "Autojump" 'helm-autojump-source))

  (spacemacs/set-leader-keys "oj" #'(lambda () (interactive)
                                      (helm :sources 'helm-source-autojump
                                            :buffer "*helm autojump*"
                                            :ff-transformer-show-only-basename nil))))
