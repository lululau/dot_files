(require 'agent-shell)

;;;###autoload
(defun lx/agent-shell-select-session ()
    (interactive)
    (agent-shell--dwim :switch-to-shell t))

