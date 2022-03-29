(setq rubocop-wrapper-path (format "%srubocop" (file-name-directory load-file-name)))

(message "debug: %s" rubocop-wrapper-path)

(with-eval-after-load 'flycheck
  (defconst flycheck-ruby-rubocop-error-patterns
    '((info line-start (optional (file-name)) ":" line ":" column ": C: "
            (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
      (warning line-start (optional (file-name)) ":" line ":" column ": W: "
              (optional (id (one-or-more (not (any ":")))) ": ") (message)
              line-end)
      (error line-start (optional (file-name)) ":" line ":" column ": " (or "E" "F") ": "
            (optional (id (one-or-more (not (any ":")))) ": ") (message)
            line-end)))


  (flycheck-define-command-checker 'ruby-rubocop
    "A Ruby syntax and style checker using the RuboCop tool.

  You need at least RuboCop 0.34 for this syntax checker.

  See URL `https://rubocop.org/'."
    ;; ruby-standard is defined based on this checker
    :command `(,rubocop-wrapper-path
              "--display-cop-names"
              "--force-exclusion"
              "--format" "emacs"
              ;; Explicitly disable caching to prevent Rubocop 0.35.1 and earlier
              ;; from caching standard input.  Later versions of Rubocop
              ;; automatically disable caching with --stdin, see
              ;; https://github.com/flycheck/flycheck/issues/844 and
              ;; https://github.com/bbatsov/rubocop/issues/2576
              "--cache" "false"
              (config-file "--config" flycheck-rubocoprc)
              (option-flag "--lint" flycheck-rubocop-lint-only)
              ;; Rubocop takes the original file name as argument when reading
              ;; from standard input
              "--stdin" source-original)
    :standard-input t
    :working-directory #'flycheck-ruby--find-project-root
    :error-patterns flycheck-ruby-rubocop-error-patterns
    :modes '(enh-ruby-mode ruby-mode)
    :next-checkers '((warning . ruby-reek)
                    (warning . ruby-rubylint))))
