(with-eval-after-load 'dired-subtree
  (require 'cl-lib)

  (defface my-dired-subtree-guide-face
    '((t :inherit shadow :weight normal :slant normal :underline nil))
    "Face used for drawing dired subtree guide lines."
    :group 'dired-subtree)

  (defun my-dired-subtree-align-all ()
    "在 dired-subtree 展开时，强制将 Dired 缓冲区中的所有文件属性列对齐到最大宽度。"
    (interactive)
    (if (fboundp 'dired--align-all-files)
        ;; Emacs 29+：使用内置对齐函数，但临时修正 dired--need-align-p 的判断逻辑
        (cl-letf* (((symbol-function 'dired--need-align-p)
                    (lambda ()
                      (save-excursion
                        (goto-char (point-min))
                        (let (distances)
                          (while (not (eobp))
                            (when (dired-move-to-filename)
                              (push (- (point) (line-beginning-position)) distances))
                            (forward-line 1))
                          (when distances
                            (let ((target (apply #'max distances)))
                              (when (cl-some (lambda (d) (/= d target)) distances)
                                target))))))))
          (dired--align-all-files))
      ;; Emacs 28 及以下版本的降级兼容逻辑
      (let ((max-dist 0)
            (regexp directory-listing-before-filename-regexp))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (dired-move-to-filename)
              (setq max-dist (max max-dist (- (point) (line-beginning-position)))))
            (forward-line 1)))
        (when (> max-dist 0)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (dired-move-to-filename)
                (let ((distance (- max-dist (- (point) (line-beginning-position))))
                      (inhibit-read-only t))
                  (unless (zerop distance)
                    (re-search-backward regexp nil t)
                    (goto-char (match-beginning 0))
                    (search-backward-regexp "[[:space:]]" nil t)
                    (skip-chars-forward "[:space:]")
                    (insert-char 32 distance 'inherit))))
              (forward-line 1)))))))

  ;; 1. 禁用 dired-subtree 默认的统一缩进
  (setq dired-subtree-line-prefix "")

  ;; 2. 获取 POS 处的子树嵌套深度
  (defun my-dired-subtree-get-depth-at-pos (pos)
    "获取 POS 处的子树嵌套深度。"
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'dired-subtree-depth))
                (overlays-at pos))))
      (if ovs
          (apply #'max (mapcar (lambda (ov) (overlay-get ov 'dired-subtree-depth)) ovs))
        0)))

  ;; 3. 清理当前 Buffer 中我们自己绘制的树形 Overlay
  (defun my-dired-subtree-clear-tree-overlays ()
    "清除当前 Buffer 中所有的自定义树形前缀 overlay。"
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'my-dired-tree-overlay)
        (delete-overlay ov))))

  ;; 4. 自底向上 O(N) 高效画树函数（在每个文件名之前精准绘制树形前缀）
  (defun my-dired-subtree-draw-tree ()
    "自底向上扫描 dired buffer，在每个文件名之前精准绘制树形前缀。"
    (interactive)
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t)
            (has-siblings (make-vector 64 nil))
            (lines-to-draw nil))
        (my-dired-subtree-clear-tree-overlays)

        ;; 第一步：自顶向下收集每行的范围、文件名起始位置和嵌套深度
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((fn-beg (dired-move-to-filename)))
              (when fn-beg
                (let ((depth (min (my-dired-subtree-get-depth-at-pos (point)) 63)))
                  (push (list (line-beginning-position) fn-beg depth) lines-to-draw))))
            (forward-line 1)))

        ;; 第二步：自底向上反向处理每一行 (O(N))
        (dolist (line-info lines-to-draw)
          (let* ((beg (car line-info))
                 (fn-beg (cadr line-info))
                 (depth (caddr line-info))
                 (parts nil))

            ;; 只有深度大于 0 的子节点才绘制树形前缀
            (when (> depth 0)
              (dotimes (i depth)
                (if (< i (1- depth))
                    (if (aref has-siblings (1+ i))
                        (push "│   " parts)
                      (push "    " parts))
                  (if (aref has-siblings (1+ i))
                      (push "├── " parts)
                    (push "└── " parts))))

              (let ((d-idx depth))
                (while (< d-idx (length has-siblings))
                  (aset has-siblings d-idx nil)
                  (setq d-idx (1+ d-idx))))
              (aset has-siblings depth t)

              (let ((prefix (propertize (apply #'concat (nreverse parts))
                                        'face 'my-dired-subtree-guide-face)))
                ;; 在文件名开头创建 1 字符宽的 overlay，将树状前缀插入其 before-string
                (let ((ov-prefix (make-overlay fn-beg (1+ fn-beg))))
                  (overlay-put ov-prefix 'my-dired-tree-overlay t)
                  (overlay-put ov-prefix 'before-string prefix)
                  (overlay-put ov-prefix 'evaporate t)))))))))

  ;; 将自动对齐函数和画树函数绑定到 dired-subtree 的操作钩子上
  (add-hook 'dired-subtree-after-insert-hook #'my-dired-subtree-align-all)
  (add-hook 'dired-subtree-after-insert-hook #'my-dired-subtree-draw-tree)
  (add-hook 'dired-subtree-after-remove-hook #'my-dired-subtree-draw-tree)
  (add-hook 'dired-after-readin-hook #'my-dired-subtree-draw-tree))
