(defun org-protocol-capture-html--with-pandoc-patch (template title url body)
  (let* ((url (lx/refine-web-string url))
         (title (lx/refine-web-string title))
         (body (lx/refine-web-string body)))
    (org-protocol-capture-html--with-pandoc (list :template template
                                                  :url url
                                                  :title title
                                                  :body body))))

(provide 'org-protocol-capture-html-patch)
