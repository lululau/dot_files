;; -*- lexical-binding: t; -*-

;;;###autoload
(defun lx/dired-rpc--ls-lisp-switches (switches)
  "Normalize GNU ls SWITCHES into a form ls-lisp parses correctly.
tramp-rpc generates Dired listings via ls-lisp emulation, but
ls-lisp--sanitize-switches has two pitfalls: any long option with
an argument (--xxx=yyy) wipes everything from it to the end of the
string, and \"--sort=none \" is wrongly converted to -S (size
sort).  Convert such options up front instead."
  (let ((s switches))
    ;; ls-lisp always prints file names literally; this option is a
    ;; no-op there but triggers the wipe described above.
    (setq s (replace-regexp-in-string "\\(?:\\`\\| \\)--quoting-style=[^ ]*" "" s))
    ;; GNU ls lets a later -t/-S override --sort=none; ls-lisp has no
    ;; equivalent, so drop it and let the remaining flags decide.
    (setq s (replace-regexp-in-string "\\(?:\\`\\| \\)--sort=none\\(?:\\'\\| \\)" " " s))
    ;; Remaining --sort=xxx become the short flags ls-lisp supports.
    (setq s (replace-regexp-in-string "--sort=time" "-t" s))
    (setq s (replace-regexp-in-string "--sort=size" "-S" s))
    (setq s (replace-regexp-in-string "--sort=extension" "-X" s))
    (setq s (replace-regexp-in-string "--sort=version" "-v" s))
    ;; Safety net: drop any other argument-taking long option.
    (setq s (replace-regexp-in-string "\\(?:\\`\\| \\)--[a-z][a-z-]*=[^ ]*" "" s))
    (string-trim (replace-regexp-in-string "  +" " " s))))

;;;###autoload
(defun lx/dired-rpc-switches-fix ()
  "Normalize switches of RPC Dired buffers for ls-lisp parsing.
See `lx/dired-rpc--ls-lisp-switches'."
  (when (and (derived-mode-p 'dired-mode)
             (file-remote-p default-directory)
             (string= (file-remote-p default-directory 'method) "rpc")
             dired-actual-switches)
    (setq dired-actual-switches
          (lx/dired-rpc--ls-lisp-switches dired-actual-switches))))
