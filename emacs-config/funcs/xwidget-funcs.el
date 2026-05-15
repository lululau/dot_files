(defun lx/xwidget-browse-url (url)
  "Browse URL in xwidget-webkit and switch to session buffer"
  (xwidget-webkit-browse-url url)
  (if (bound-and-true-p xwidget-webkit-last-session-buffer)
      (switch-to-buffer xwidget-webkit-last-session-buffer)))

;;;###autoload
(defun lx/xwidget-open-local-clash ()
  "Open local clash dashboard"
  (interactive)
  (lx/xwidget-browse-url "http://127.0.0.1:9090/ui/#/proxies"))

;;;###autoload
(defun lx/xwidget-open-remote-clash ()
  "Open remote clash dashboard"
  (interactive)
  (lx/xwidget-browse-url "http://10.10.10.1:9090/ui/dashboard/#/proxies"))

;;;###autoload
(defun lx/xwidget-open-openclash ()
  "Open OpenClash LuCI"
  (interactive)
  (lx/xwidget-browse-url "http://10.10.10.1/cgi-bin/luci/admin/services/openclash/config"))

;;;###autoload
(defun lx/xwidget-open-github ()
  "Open GitHub"
  (interactive)
  (lx/xwidget-browse-url "https://github.com"))

;;;###autoload
(defun lx/xwidget-open-gitlab ()
  "Open GitLab"
  (interactive)
  (lx/xwidget-browse-url "https://gitlab.upeastscm.com"))

;;;###autoload
(defun lx/xwidget-open-jenkins ()
  "Open Jenkins"
  (interactive)
  (lx/xwidget-browse-url "https://jenkins.ktjr.com"))

;;;###autoload
(defun lx/xwidget-open-jira ()
  "Open Jira"
  (interactive)
  (lx/xwidget-browse-url "https://jira.ktjr.com"))

;;;###autoload
(defun lx/xwidget-open-v2ex ()
  "Open V2EX"
  (interactive)
  (lx/xwidget-browse-url "https://www.v2ex.com"))

;;;###autoload
(defun lx/xwidget-open-emacs-china ()
  "Open Emacs China"
  (interactive)
  (lx/xwidget-browse-url "https://emacs-china.org"))

;;;###autoload
(defun lx/xwidget-open-twitter ()
  "Open Twitter"
  (interactive)
  (lx/xwidget-browse-url "https://twitter.com/home"))
