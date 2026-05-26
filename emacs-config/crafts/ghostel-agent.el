(require 'run-in-ghostel)


(defun ghostel-agent-run-claude ()
  "Start a Claude agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "claude --dangerously-skip-permissions" "*ghostel-claude[%p]*"))

(defun ghostel-agent-run-opencode()
  "Start an Opencode agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "opencode" "*ghostel-opencode[%p]*"))

(defun ghostel-agent-run-cursor ()
  "Start a Cursor agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "cursor-agent --force" "*ghostel-cursor[%p]*"))

(defun ghostel-agent-run-antigravity ()
  "Start an Antigravity agent shell in Ghostel."
  (interactive)
  (lx/run-in-projectile-ghostel "agy --dangerously-skip-permissions" "*ghostel-agy[%p]*"))

(provide 'ghostel-agent)
