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

  (defun ssh-tunnels-run (&optional arg)
    (interactive "P")
    (let ((tunnel (ssh-tunnels--tunnel t)))
      (when (numberp arg)
        (setf tunnel (cl-list* :local-port arg tunnel)))
      (when (not (ssh-tunnels--check tunnel))
        (message "Tunneling...")
        (ssh-tunnels--run tunnel)
        (let ((name (ssh-tunnels--property tunnel :name))
              (local-port (ssh-tunnels--property tunnel :local-port)))
          (message "Tunnel '%s' on port %d" name local-port))))
    (forward-line))

  (defun ssh-tunnels-kill ()
    (interactive)
    (let ((tunnel (ssh-tunnels--tunnel t)))
      (when (ssh-tunnels--check tunnel)
        (ssh-tunnels--kill tunnel)
        (message "Tunnel '%s' killed" (ssh-tunnels--property tunnel :name))))
    (forward-line))

  (defun ssh-tunnels-refresh-without-check ()
    (interactive)
    (let ((name-width ssh-tunnels-name-width)
          (local-port-width ssh-tunnels-local-port-width)
          (host-width ssh-tunnels-host-width)
          (remote-port-width ssh-tunnels-remote-port-width)
          (login-width ssh-tunnels-login-width))
      (setq tabulated-list-format
            (vector `("S" 1 t)
                    `("Name" ,name-width t)
                    `("T." 2 t)
                    `("LPort" ,local-port-width ssh-tunnels--lport> :right-align t)
                    `("Host" ,host-width t)
                    `("RPort" ,remote-port-width ssh-tunnels--rport> :right-align t)
                    `("Login" ,login-width t))))
    (setq tabulated-list-use-header-line ssh-tunnels-use-header-line)
    (let ((entries '()))
      (dolist (tunnel ssh-tunnels-configurations)
        (let* ((name (ssh-tunnels--property tunnel :name))
               (tunnel-type (ssh-tunnels--property tunnel :type))
               (local-port (ssh-tunnels--property tunnel :local-port))
               (host (ssh-tunnels--property tunnel :host))
               (remote-port (ssh-tunnels--property tunnel :remote-port))
               (login (ssh-tunnels--property tunnel :login)))
          (push (list tunnel
                      (vector " "
                              (ssh-tunnels--pretty-name name)
                              tunnel-type
                              (number-to-string local-port)
                              host
                              (number-to-string remote-port)
                              login))
                entries)))
      (setq tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t))


  (defun ssh-tunnels ()
    "View and manipulate SSH tunnels."
    (interactive)
    (switch-to-buffer (let ((buffer (get-buffer-create "*SSH tunnels*")))
                        (with-current-buffer buffer
                          (ssh-tunnels-mode)
                          (ssh-tunnels-refresh-without-check))
                        buffer)))
  )
