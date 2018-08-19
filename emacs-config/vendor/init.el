(let ((dir (file-name-directory load-file-name)))
  (autoload 'org-protocol-capture-html--with-pandoc (format "%sorg-protocol-capture-html.el" dir))
  (autoload 'toggle-company-english-helper (format "%scompany-english-helper.el" dir)))
