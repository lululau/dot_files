(advice-add
 'spacemacs//helm-perspectives-source
 :override
 (lambda ()
   (helm-build-sync-source
       (concat "Current Perspective: " (spacemacs//current-layout-name))
     :candidates (--map (cons (f-filename it) it) (persp-names))
     :fuzzy-match t
     :action
     '(("Switch to perspective" . persp-switch)
       ("Close perspective(s)" . (lambda (candidate)
                                   (mapcar
                                    'persp-kill-without-buffers
                                    (helm-marked-candidates))))
       ("Kill perspective(s)" . (lambda (candidate)
                                  (mapcar 'persp-kill
                                          (helm-marked-candidates))))))))
