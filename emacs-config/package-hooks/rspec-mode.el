(spacemacs|use-package-add-hook rspec-mode
  :post-config

  (defun rspec-core-options ()
    "Return string of options that instructs spec to use options
file if it exists, or sensible defaults otherwise."
    (cond ((and rspec-use-opts-file-when-available
                (file-readable-p (rspec-spec-opts-file)))
           (concat "--options " (rspec--shell-quote-local (rspec-spec-opts-file)) (if (bound-and-true-p rspec-fail-fast) " --fail-fast" "")))
          (t rspec-command-options)))

  (defun rspec-toggle-fail-fast()
    (interactive)
    (setq rspec-fail-fast (not (bound-and-true-p rspec-fail-fast)))
    (message "Turn rspec --fail-fast option %s" (if (bound-and-true-p rspec-fail-fast) "on" "off")))

  (define-key rspec-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example)
  (define-key ruby-mode-map [M-s-tab] #'rspec-toggle-spec-and-target-find-example)
  (spacemacs/set-leader-keys-for-major-mode 'ruby-mode
    "tF"    'rspec-toggle-fail-fast)
  (spacemacs/set-leader-keys-for-major-mode 'rspec-compilation-mode
    "tl"    #'(lambda () (interactive) (with-selected-window (cadr (window-list)) (call-interactively 'rspec-run-last-failed)))
    "tr"    #'(lambda () (interactive) (with-selected-window (cadr (window-list)) (call-interactively 'rspec-rerun)))))
