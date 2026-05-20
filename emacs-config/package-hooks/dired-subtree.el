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

  ;; 将自动对齐函数加入钩子中
  (add-hook 'dired-subtree-after-insert-hook #'my-dired-subtree-align-all))
