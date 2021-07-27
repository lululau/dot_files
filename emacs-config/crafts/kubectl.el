(defun lx/kubectl/execute (command start end)
  (let* ((new-buffer (get-buffer-create "*lx/kubectl/execute/logs*")))
    (save-current-buffer
      (switch-to-buffer-other-window new-buffer)
      (end-of-buffer)
      (insert (format-time-string "\n\n%F %T\n--------------------------\n" (current-time))))
    (call-shell-region start end command nil new-buffer)))

(defun lx/kubectl/apply-region-or-buffer ()
  (interactive)
  (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max))))
    (lx/kubectl/execute "kubectl apply -f -" begin end)))

(defun lx/kubectl/delete-region-or-buffer ()
  (interactive)
  (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max))))
    (lx/kubectl/execute "kubectl delete -f -" begin end)))

(defun lx/kubectl/istio-inject-apply-region-or-buffer ()
  (interactive)
  (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max))))
    (lx/kubectl/execute "istioctl kube-inject -f - | kubectl apply -f -" begin end)))

(defun lx/kubectl/istio-inject-delete-region-or-buffer ()
  (interactive)
  (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max))))
    (lx/kubectl/execute "istioctl kube-inject -f - | kubectl delete -f -" begin end)))

(provide 'lx/kubectl)
