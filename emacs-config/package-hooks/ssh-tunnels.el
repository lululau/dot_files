(with-eval-after-load 'ssh-tunnels
  (evilified-state-evilify-map ssh-tunnels-mode-map
    :mode ssh-tunnels-mode
    :bindings
    (kbd "r") 'ssh-tunnels-run
    (kbd "R") 'ssh-tunnels-rerun
    (kbd "d") 'ssh-tunnels-kill))
