;; -*- lexical-binding: t; -*-

(with-eval-after-load 'claude-code-ide

  (defun claude-code-ide-mcp-send-file (&optional file-path)
    "Send current file notification."
    (let* ((file-path (or file-path (buffer-file-name))))
      (claude-code-ide-mcp--send-notification
       "at_mentioned"
       `((filePath . ,file-path)))))

  (defun claude-code-ide-insert-current-file ()
    "Insert current file path into Claude prompt."
    (interactive)
    (if-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
              (session (claude-code-ide-mcp--get-session-for-project project-dir))
              (client (claude-code-ide-mcp-session-client session)))
        (progn
          (claude-code-ide-mcp-send-file)
          (claude-code-ide-debug "Sent current file to Claude Code"))
      (user-error "Claude Code is not connected.  Please start Claude Code first")))

  (defun claude-code-ide-insert-select-file ()
    "Insert selected file path into Claude prompt using helm."
    (interactive)
    (if-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
              (session (claude-code-ide-mcp--get-session-for-project project-dir))
              (client (claude-code-ide-mcp-session-client session)))
        (let* ((default-directory (or (projectile-project-root) default-directory))
               (file-path (helm-get-file default-directory)))
          (when (and file-path (file-exists-p file-path))
            (claude-code-ide-mcp-send-file file-path)
            (claude-code-ide-debug "Sent selected file to Claude Code: %s" file-path)))
      (user-error "Claude Code is not connected. Please start Claude Code first")))

  (transient-define-prefix claude-code-ide-menu ()
    "Claude Code IDE main menu."
    [:description claude-code-ide--session-status]
    ["Claude Code IDE"
     ["Session Management"
      ("s" claude-code-ide--start-if-no-session :description claude-code-ide--start-description)
      ("c" claude-code-ide--continue-if-no-session :description claude-code-ide--continue-description)
      ("r" claude-code-ide--resume-if-no-session :description claude-code-ide--resume-description)
      ("q" "Stop current session" claude-code-ide-stop)
      ("l" "List all sessions" claude-code-ide-list-sessions)]
     ["Navigation"
      ("b" "Switch to Claude buffer" claude-code-ide-switch-to-buffer)
      ("w" "Toggle window visibility" claude-code-ide-toggle-window)
      ("W" "Toggle recent window" claude-code-ide-toggle-recent)]
     ["Interaction"
      ("i" "Insert selection" claude-code-ide-insert-at-mentioned)
      ("f" "Insert current file" claude-code-ide-insert-current-file)
      ("F" "Insert select file" claude-code-ide-insert-select-file)
      ("p" "Send prompt from minibuffer" claude-code-ide-send-prompt)
      ("e" "Send escape key" claude-code-ide-send-escape)
      ("n" "Insert newline" claude-code-ide-insert-newline)]
     ["Submenus"
      ("C" "Configuration" claude-code-ide-config-menu)
      ("d" "Debugging" claude-code-ide-debug-menu)]])

  )
