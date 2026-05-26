;; -*- lexical-binding: t; -*-

(defun lx/dired-rpc-switches-fix ()
  "tramp-rpc uses ls-lisp; --quoting-style=literal breaks switch parsing."
  (when (and (derived-mode-p 'dired-mode)
             (file-remote-p default-directory)
             (string= (file-remote-p default-directory 'method) "rpc"))
    (setq dired-listing-switches "-aBhl")
    (when dired-actual-switches
      (setq dired-actual-switches
            (replace-regexp-in-string "--quoting-style=literal " ""
                                      dired-actual-switches)))))
