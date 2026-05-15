;;;###autoload
(defun vi/del-org-props ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute "g/:PROPERTIES:/.,/:END:/normal dd")))


;;;###autoload
(defun vi/convert-org-example-to-src (lang)
  (interactive "sLanguage: ")
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute (format "%%s/#\\+begin_example\\C/#+begin_src %s" lang))
    (evil-ex-execute (format "%%s/#\\+BEGIN_EXAMPLE\\C/#+BEGIN_SRC %s" lang))
    (evil-ex-execute "%s/#\\+end_example/#+end_src" )))

;;;###autoload
(defun vi/convert-org-src-to-example ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute "%s/#\\+begin_src.*\\C/#+begin_example")
    (evil-ex-execute "%s/#\\+BEGIN_SRC.*\\C/#+BEGIN_EXAMPLE")
    (evil-ex-execute "%s/#\\+end_src/#+end_example" )))

;;;###autoload
(defun vi/strip-ansi-code ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute  "%s/\[[0-9;]*m//g")))

;;;###autoload
(defun vi/del-github-data-uri ()
  (interactive)
  (let ((evil-ex-current-buffer (current-buffer)))
    (evil-ex-execute  "%s/\\[\\[data:image[^]]+\\]\\]//g")
    (evil-ex-execute  "%s/^\\(\\*+\\s-+\\)\\[\\[.*\\]/\\1/")
    (evil-ex-execute  "%s/\\[\\[\\([^]]+\\)\\]\\[\\[\\[\\([^]]+\\)\\]\\]\\]\\]/[[\\2]]/g")))

;;;###autoload
(defun lx/evil-next-10-lines ()
  "Move cursor down 10 lines"
  (interactive)
  (evil-next-line 10))

;;;###autoload
(defun lx/evil-previous-10-lines ()
  "Move cursor up 10 lines"
  (interactive)
  (evil-previous-line 10))

;;;###autoload
(defun lx/evil-next-10-lines ()
  "Move cursor down 10 lines"
  (interactive)
  (evil-next-line 10))

;;;###autoload
(defun lx/evil-substitute-and-indent ()
  "Evil substitute then indent"
  (interactive)
  (call-interactively 'evil-substitute)
  (call-interactively 'indent-for-tab-command))

;;;###autoload
(defun lx/evil-smart-toggle-fold ()
  "Toggle fold. Use web-mode-fold-or-unfold in web-mode"
  (interactive)
  (if (eq major-mode 'web-mode)
      (web-mode-fold-or-unfold)
    (evil-toggle-fold)))

;;;###autoload
(defun lx/evil-smart-goto-file ()
  "Goto file at point. Use projectile-rails in ruby-mode"
  (interactive)
  (if (and (eq 'ruby-mode major-mode) projectile-rails-mode)
      (call-interactively 'projectile-rails-goto-file-at-point)
    (call-interactively 'ffap-other-window)))

;;;###autoload
(defun lx/evil-insert-newline-below ()
  "Insert newline below in evil"
  (interactive)
  (evil-insert-newline-below))
