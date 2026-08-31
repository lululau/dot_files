;; -*- lexical-binding: t; -*-

;; ~/.hermes/kanban.db 是 SQLite WAL 库，hermes gateway 每次开关连接都会
;; 创建/删除 kanban.db-shm、kanban.db-wal。global-auto-revert 对停在
;; ~/.hermes 的 dired buffer 频繁重跑 gls，与文件删除撞出竞态时，
;; gls 的 stderr 会写进 "*ls error*"（cannot access ... No such file）。
;; 对该目录下的 dired buffer 关闭 autorevert；手动 g 刷新不受影响。
(with-eval-after-load 'dired
  (defun lx/dired-no-autorevert-under-hermes ()
    "Stop global auto-revert from reverting dired buffers under ~/.hermes."
    (when (string-prefix-p (expand-file-name "~/.hermes/")
                           (expand-file-name default-directory))
      (setq-local buffer-stale-function #'ignore)))

  (add-hook 'dired-mode-hook #'lx/dired-no-autorevert-under-hermes))
