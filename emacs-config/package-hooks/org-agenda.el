(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "M") #'org-agenda-month-view)

  (advice-add 'org-agenda-list :before #'(lambda (&rest args)
                                           (require 'org-journal)
                                           (unless (bound-and-true-p org-super-agenda-mode)
                                             (org-super-agenda-mode)))))
