(require 'agent-shell)

;;;###autoload
(defun lx/agent-shell-select-session ()
  (interactive)
  (agent-shell--dwim :switch-to-shell t))


;;;###autoload
(defun lx/agent-shell-in-other-window (&optional arg)
  "Start or reuse an agent shell in another window.

Same as `agent-shell', but always displays the shell in another window.
When the current frame has only one window, split it first.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
  (interactive "P")
  (setq lx/agent-shell--display-in-other-window t)
  (condition-case-unless-debug err
      (if arg
          (agent-shell--dwim)
        (let ((agent-shell-preferred-agent-config 'claude-code))
          (agent-shell--dwim)))
    (quit (setq lx/agent-shell--display-in-other-window nil))))
