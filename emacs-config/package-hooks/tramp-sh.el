(with-eval-after-load 'tramp-sh
  (defun tramp-get-ls-command (vec)
    (with-tramp-connection-property vec "ls"
      (tramp-message vec 5 "Finding a suitable `ls' command")
      (or
       (catch 'ls-found
         (dolist (cmd '("gnuls" "gls" "ls"))
           (let ((dl (tramp-get-remote-path vec))
                 result)
             (while (and dl (setq result (tramp-find-executable vec cmd dl t t)))
               ;; Check parameters.  On busybox, "ls" output coloring is
               ;; enabled by default sometimes.  So we try to disable it
               ;; when possible.  $LS_COLORING is not supported there.
               ;; Some "ls" versions are sensible wrt the order of
               ;; arguments, they fail when "-al" is after the
               ;; "--color=never" argument (for example on FreeBSD).
               (when (tramp-send-command-and-check
                      vec (format "%s -lnd /" result))
                 (when (tramp-send-command-and-check
                        vec (format
                             "%s --color=never -al /dev/null" result))
                   (setq result (concat result " --color=never")))
                 (throw 'ls-found result))
               (setq dl (cdr dl))))))
       (tramp-error vec 'file-error "Couldn't find a proper `ls' command")))))
