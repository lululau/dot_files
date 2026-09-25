;;; update-packages-and-restart.el --- one-key package update with auto restart  -*- lexical-binding: t; -*-

;;; Commentary:
;; 一键更新软件包并重启 Emacs 完成安装，全程无确认：
;;
;;   M-x lx/update-packages-and-restart
;;
;; 流程：configuration-layer/update-packages 免确认执行（备份并删除旧包）
;; → 若确有更新则静默保存所有可保存 buffer，跳过全部退出确认
;; （未保存 buffer、运行中的进程、ghostel/persp/server 查询），重启 Emacs。
;; 重启后 Spacemacs 会自动重装被删除的包。

;;; Code:

(defvar lx/packages-updated-pending-restart nil
  "Non-nil if `configuration-layer//update-packages' deleted packages awaiting reinstall.
Set by advice; reset only by restarting Emacs.")

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
  "Update packages with no confirmation, then auto-restart Emacs to install them."
  (interactive)
  (configuration-layer/update-packages t)
  (if lx/packages-updated-pending-restart
      (progn
        (message "Packages updated; restarting Emacs to install them...")
        (lx/restart-emacs-unconditionally))
    (message "All packages are up to date; no restart needed.")))

(provide 'update-packages-and-restart)
;;; update-packages-and-restart.el ends here
