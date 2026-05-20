(with-eval-after-load 'dired-subtree
  (require 'cl-lib)

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

  ;; 1. 禁用 dired-subtree 默认的统一缩进，交由我们的自定义画树函数来绘制
  (setq dired-subtree-line-prefix "")

  ;; 2. 获取某一位置的子树嵌套深度
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

  ;; 4. 自底向上 O(N) 高效画树核心函数
  (defun my-dired-subtree-draw-tree ()
    "自底向上扫描 dired buffer，为所有行精准绘制树形前缀。"
    (interactive)
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t)
            (has-siblings (make-vector 64 nil)) ; 记录各深度下方是否还有同级节点，支持到 63 层嵌套
            (lines-to-draw nil))
        (my-dired-subtree-clear-tree-overlays)

        ;; 第一步：自顶向下收集所有代表文件的行以及它们的深度
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (dired-get-filename nil t)
              (let ((depth (min (my-dired-subtree-get-depth-at-pos (point)) 63)))
                ;; 记录行开头位置、结尾位置和深度。由于使用 push，列表将自然呈“自底向上”的反向顺序
                (push (list (line-beginning-position) (line-end-position) depth) lines-to-draw)))
            (forward-line 1)))

        ;; 第二步：自底向上反向处理每一行 (高效 O(N))
        (dolist (line-info lines-to-draw)
          (let* ((beg (car line-info))
                 (end (cadr line-info))
                 (depth (caddr line-info))
                 (parts nil))

            ;; 构造当前行的树状前缀
            (dotimes (i (1+ depth))
              (if (< i depth)
                  ;; 如果是父级列：根据下方是否还有该父级的同级节点决定画“│”还是留空
                  (if (aref has-siblings i)
                      (push "│ " parts)
                    (push "  " parts))
                ;; 如果是当前级列：根据下方是否还有同级节点决定画“├──”还是“└──”
                (if (aref has-siblings i)
                    (push "├──" parts)
                  (push "└──" parts))))

            ;; 更新兄弟节点状态：当前行之上的行在当前深度（以及更浅深度）都有同级节点了
            ;; 重置比当前深度更深的所有状态
            (let ((d-idx (1+ depth)))
              (while (< d-idx (length has-siblings))
                (aset has-siblings d-idx nil)
                (setq d-idx (1+ d-idx))))
            ;; 标记当前深度有同级节点
            (aset has-siblings depth t)

            ;; 创建 overlay 并应用计算好的 line-prefix
            (let ((prefix (apply #'concat (nreverse parts)))
                  (ov (make-overlay beg end)))
              (overlay-put ov 'my-dired-tree-overlay t)
              (overlay-put ov 'line-prefix prefix)
              (overlay-put ov 'evaporate t)))))))

  ;; 将自动对齐函数加入钩子中
  (add-hook 'dired-subtree-after-insert-hook #'my-dired-subtree-align-all)

  ;; 5. 将画树函数绑定到 dired-subtree 的操作钩子上
  (add-hook 'dired-subtree-after-insert-hook #'my-dired-subtree-draw-tree)
  (add-hook 'dired-subtree-after-remove-hook #'my-dired-subtree-draw-tree)
  (add-hook 'dired-after-readin-hook #'my-dired-subtree-draw-tree))
