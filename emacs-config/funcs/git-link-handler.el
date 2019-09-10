(defun git-link-gitlab-no-https (hostname dirname filename branch commit start end)
  (format "http://%s/%s/blob/%s/%s"
          hostname
          dirname
          (or branch commit)
          (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

