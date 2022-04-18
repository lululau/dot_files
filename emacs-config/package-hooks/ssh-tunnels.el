(with-eval-after-load 'ssh-tunnels
  (evilified-state-evilify-map ssh-tunnels-mode-map
    :mode ssh-tunnels-mode
    :bindings
    (kbd "r") 'ssh-tunnels-run
    (kbd "R") 'ssh-tunnels-rerun
    (kbd "d") 'ssh-tunnels-kill
    (kbd "q") 'quit-window
    (kbd "g") 'ssh-tunnels-refresh
    (kbd "r") 'ssh-tunnels-run
    (kbd "R") 'ssh-tunnels-rerun)

  (defun ssh-tunnels--check (tunnel)
    (let* ((name (ssh-tunnels--property tunnel :name)))
      (file-exists-p (format "/tmp/%s" name))))
  )
