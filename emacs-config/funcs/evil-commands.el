(defun vi/del-org-props ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute "g/:PROPERTIES:/.,/:END:/d")))


(defun vi/convert-org-example-to-src (lang)
  (interactive "sLanguage: ")
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (format "%%s/#\\+begin_example\\C/#+begin_src %s" lang))
    (evil-ex-execute (format "%%s/#\\+BEGIN_EXAMPLE\\C/#+BEGIN_SRC %s" lang))
    (evil-ex-execute "%s/#\\+end_example/#+end_src" )))
