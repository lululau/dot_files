;;; update-packages-and-restart.el --- one-key package update with auto restart  -*- lexical-binding: t; -*-

;;; Commentary:
;; 一键更新软件包并重启 Emacs 完成安装，全程无确认：
;;
;;   M-x lx/update-packages-and-restart
;;   emacsclient -e '(lx/update-packages-and-restart)'
;;
;; 命令返回更新报告字符串（更新了哪些包、旧->新版本、备份目录），
;; emacsclient -e 会把它打印到 stdout，供 Agent 等外部调用方读取。
;; 重启由 `lx/update-packages-restart-delay' 延时异步触发，确保调用方
;; 先拿到返回值；重启前静默保存所有可保存 buffer，并跳过全部退出确认
;; （未保存 buffer、运行中的进程、ghostel/persp/server 查询）。
;; 重启后 Spacemacs 会自动重装被删除的包。

;;; Code:

(defvar lx/packages-updated-pending-restart nil
  "Non-nil if `configuration-layer//update-packages' deleted packages awaiting reinstall.
Set by advice; reset only by restarting Emacs.")

(defvar lx/package-update-report nil
  "Alist of (PKG OLD-VERSION NEW-VERSION) captured at update time.
OLD-VERSION comes from `package-alist' before deletion; NEW-VERSION is the
highest version available in the refreshed `package-archive-contents'.")

(defvar lx/update-packages-restart-delay 2
  "Seconds to delay the restart so `emacsclient -e' can print the report first.")

(defun lx/package-installed-version (pkg)
  "Return installed version string of PKG, or nil."
  (when-let* ((entry (assq pkg package-alist)))
    (package-version-join (package-desc-version (cadr entry)))))

(defun lx/package-archive-version (pkg)
  "Return highest available version string of PKG from archives, or nil."
  (when-let* ((descs (cdr (assq pkg package-archive-contents))))
    (let (best)
      (dolist (d (if (package-desc-p descs) (list descs) descs))
        (when (or (null best)
                  (version-list-< (package-desc-version best)
                                  (package-desc-version d)))
          (setq best d)))
      (package-version-join (package-desc-version best)))))

(defun lx/latest-rollback-dir ()
  "Return the newest rollback snapshot directory, or nil."
  (let* ((dirs (directory-files configuration-layer-rollback-directory t
                                directory-files-no-dot-files-regexp))
         (dirs (cl-remove-if-not #'file-directory-p dirs)))
    (car (sort dirs
               (lambda (a b)
                 (time-less-p (nth 5 (file-attributes b))
                              (nth 5 (file-attributes a))))))))

(defun lx/package-update-report-string ()
  "Build the update report string from `lx/package-update-report'."
  (if (and lx/package-update-report
           (cl-some #'identity (mapcar #'cdr lx/package-update-report)))
      (format "Updated %d package(s): %s; old versions backed up in: %s; Emacs is restarting to install them."
              (length lx/package-update-report)
              (mapconcat (lambda (e)
                           (format "%s %s->%s"
                                   (car e) (or (nth 1 e) "?") (or (nth 2 e) "?")))
                         lx/package-update-report ", ")
              (or (lx/latest-rollback-dir) "?"))
    "No package updates; all packages are up to date."))

(advice-add #'configuration-layer//update-packages
            :before
            (lambda (update-packages &rest _)
              (setq lx/package-update-report
                    (mapcar (lambda (pkg)
                              (list pkg
                                    (lx/package-installed-version pkg)
                                    (lx/package-archive-version pkg)))
                            update-packages))))

(advice-add #'configuration-layer//update-packages :after
            (lambda (&rest _)
              (setq lx/packages-updated-pending-restart t)))

;;;###autoload
(defun lx/restart-emacs-unconditionally ()
  "Save all savable buffers, then restart Emacs without any exit confirmation."
  (interactive)
  (save-some-buffers t)
  (let ((confirm-kill-processes nil)
        (confirm-kill-emacs nil)
        (kill-emacs-query-functions nil))
    (spacemacs/restart-emacs)))

;;;###autoload
(defun lx/update-packages-and-restart ()
  "Update packages with no confirmation, then restart Emacs to install them.
Return a report string (updated packages, old->new versions, backup
directory).  When called via `emacsclient -e' the report goes to stdout;
the restart itself is deferred by `lx/update-packages-restart-delay'
seconds so the caller receives the report before Emacs exits."
  (interactive)
  (configuration-layer/update-packages t)
  (let ((report (lx/package-update-report-string)))
    (if lx/packages-updated-pending-restart
        (progn
          (message "Packages updated; restarting Emacs in %ss..."
                   lx/update-packages-restart-delay)
          (run-at-time lx/update-packages-restart-delay nil
                       #'lx/restart-emacs-unconditionally)
          report)
      report)))

(provide 'update-packages-and-restart)
;;; update-packages-and-restart.el ends here
