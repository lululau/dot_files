;; -*- lexical-binding: t; -*-

(with-eval-after-load 'clutch
    ;; Force evilified state for special-mode-derived buffers
    (evil-set-initial-state 'clutch-result-mode 'evilified)
    (evil-set-initial-state 'clutch-record-mode 'evilified)
    ;; (evil-set-initial-state 'clutch-repl-mode 'evilified)

    ;; Result browser: parent is special-mode, evilify it
    (evilified-state-evilify-map clutch-result-mode-map
      :mode clutch-result-mode
      :bindings
      "RET"      'clutch-result-open-record
      "TAB"      'clutch-result-next-cell
      "<backtab>"'clutch-result-prev-cell
      "n"        'clutch-result-down-cell
      "p"        'clutch-result-up-cell
      "N"        'clutch-result-next-page
      "P"        'clutch-result-prev-page
      "M->"      'clutch-result-last-page
      "M-<"      'clutch-result-first-page
      "#"        'clutch-result-count-total
      "A"        'clutch-result-aggregate
      "]"        'clutch-result-scroll-right
      "["        'clutch-result-scroll-left
      "="        'clutch-result-widen-column
      "-"        'clutch-result-narrow-column
      "W"        'clutch-result-apply-filter
      "/"        'clutch-result-filter
      "s"        'clutch-result-sort-by-column
      "S"        'clutch-result-sort-by-column-desc
      "C"        'clutch-result-goto-column
      "?"        'clutch-result-column-info
      "g"        'clutch-result-rerun
      "c"        'clutch-result-copy-dispatch
      "v"        'clutch-result-view-value
      "e"        'clutch-result-export
      "f"        'clutch-result-fullscreen-toggle
      "d"        'clutch-result-delete-rows
      "i"        'clutch-result-insert-row
      "I"        'clutch-clone-row-to-insert)

    ;; Record view: parent is special-mode, evilify it
    (evilified-state-evilify-map clutch-record-mode-map
      :mode clutch-record-mode
      :bindings
      "RET" 'clutch-record-toggle-expand
      "n"   'clutch-record-next-row
      "p"   'clutch-record-prev-row
      "v"   'clutch-record-view-value
      "I"   'clutch-clone-row-to-insert
      "g"   'clutch-record-refresh
      "q"   'quit-window)

    ;; REPL mode: parent is comint-mode, evilify it
    (evilified-state-evilify-map clutch-repl-mode-map
      :mode clutch-repl-mode
      :bindings
      "C-c C-e" 'clutch-connect
      "C-c C-m" 'clutch-commit
      "C-c C-u" 'clutch-rollback
      "C-c C-a" 'clutch-toggle-auto-commit
      "C-c C-j" 'clutch-jump
      "C-c C-d" 'clutch-describe-dwim
      "C-c C-o" 'clutch-act-dwim
      "C-c C-l" 'clutch-switch-schema)

    ;; Query console (clutch-mode) inherits sql-mode which is a major mode.
    ;; Use evil-collection-define-key to bind C-c prefixed keys in
    ;; evilified state so they don't get swallowed by evil.
    (evil-collection-define-key 'evilified 'clutch-mode-map
      "C-c C-c" 'clutch-execute-dwim
      "C-c C-r" 'clutch-execute-region
      "C-c C-b" 'clutch-execute-buffer
      "C-c C-e" 'clutch-connect
      "C-c C-m" 'clutch-commit
      "C-c C-u" 'clutch-rollback
      "C-c C-a" 'clutch-toggle-auto-commit
      "C-c C-j" 'clutch-jump
      "C-c C-d" 'clutch-describe-dwim
      "C-c C-o" 'clutch-act-dwim
      "C-c C-l" 'clutch-switch-schema
      "C-c C-p" 'clutch-preview-execution-sql
      "C-c C-s" 'clutch-refresh-schema
      "C-c ?"   'clutch-dispatch))
