;; -*- lexical-binding: t; -*-

(spacemacs|use-package-add-hook company
  :post-config
  (define-key company-active-map (kbd "C-r") 'helm-company)
  (when (eq ruby-backend 'robe)
    (push 'company-robe company-backends))
  ;; (add-to-list 'company-backends #'company-tabnine)
  (spacemacs|add-company-backends
    :backends company-indirect-sql-backend
    :modes sql-mode)
  (define-key company-mode-map (kbd "<backtab>") 'copilot-accept-completion)
  (define-key company-mode-map (kbd "M-f") 'lx/copilot-accept-or-forward-word)
  (define-key company-mode-map (kbd "C-n") 'lx/copilot-accept-or-next-line)
  (define-key company-active-map (kbd "C-l") 'yas/expand)
  (define-key company-active-map (kbd "C-y") '(lambda () (interactive) (company-abort) (call-interactively 'company-yasnippet)))
  (add-hook 'company-after-completion-hook '(lambda (&optional args) (copilot-clear-overlay)))

  )

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends)
  ;; Spacemacs windows-scripts 层在 post-init-company 中把 bmx 的 hook 加到了
  ;; *全局* company-completion-finished-hook，但 bmx-mode 只在 bat-mode 时才加载，
  ;; 导致任意 buffer 补全完成时报 void-function，company-cancel 中断，
  ;; 后续 post-completion（如 yasnippet 展开）被跳过。改为仅在 bmx-mode buffer 内局部生效。
  (remove-hook 'company-completion-finished-hook #'bmx--company-completion-finished-hook))

(add-hook 'bmx-mode-hook
          (lambda ()
            (add-hook 'company-completion-finished-hook
                      #'bmx--company-completion-finished-hook nil t)))

(with-eval-after-load 'company-box
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-octicon "file-text" :height 0.8 :v-adjust -0.05))
          (Text . ,(all-the-icons-faicon "file-text-o" :height 0.8 :v-adjust -0.0575))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.0575))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.0575))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.0575))
          (Field . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Variable . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Class . ,(all-the-icons-faicon "cog" :height 0.8 :v-adjust -0.0575))
          (Interface . ,(all-the-icons-faicon "cogs" :height 0.8 :v-adjust -0.0575))
          (Module . ,(all-the-icons-alltheicon "less" :height 0.8))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.0575))
          (Unit . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Value . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Enum . ,(all-the-icons-faicon "file-text-o" :height 0.8 :v-adjust -0.0575))
          (Keyword . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.225))
          (Snippet . ,(all-the-icons-material "content_paste" :height 0.8 :v-adjust -0.225))
          (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.225))
          (File . ,(all-the-icons-faicon "file" :height 0.8 :v-adjust -0.0575))
          (Reference . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Folder . ,(all-the-icons-faicon "folder" :height 0.8 :v-adjust -0.0575))
          (EnumMember . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Constant . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (Struct . ,(all-the-icons-faicon "cog" :height 0.8 :v-adjust -0.0575))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.0575))
          (Operator . ,(all-the-icons-faicon "tag" :height 0.8 :v-adjust -0.0575))
          (TypeParameter . ,(all-the-icons-faicon "cog" :height 0.8 :v-adjust -0.0575))
          (Template . ,(all-the-icons-octicon "file-code" :height 0.8 :v-adjust -0.05)))))

(with-eval-after-load 'company-box
  ;; company 20261107 起 GUI 下默认前端变为内置的 `company-childframe-unless-just-one-frontend'，
  ;; 而 company-box 只会移除 pseudo-tooltip 前端，导致两个 childframe 前端同时工作。
  ;; 启用 company-box 时把内置 childframe 前端从 buffer-local 的 company-frontends 中去掉。
  (define-advice company-box--set-mode (:after (&rest _) lx/drop-builtin-childframe)
    (when (bound-and-true-p company-box-mode)
      (setq company-frontends
            (seq-remove (lambda (f) (memq f '(company-childframe-frontend
                                              company-childframe-unless-just-one-frontend)))
                        company-frontends))))

  ;; `company-box--handle-scroll-parent' 挂在 window-scroll-functions 上，只检查窗口
  ;; 是否为弹窗来源，不检查 company 状态。窗口滚动时若补全已被清空
  ;; （company-candidates-length 为 nil）或前缀已滚出可见区（posn-at-point 返回 nil），
  ;; `company-box--compute-frame-position' 会对 nil 做算术，报
  ;; (wrong-type-argument number-or-marker-p nil)。这里在这两种情况下跳过重定位。
  (define-advice company-box--handle-scroll-parent (:before-while (win _new-start) lx/guard)
    (and company-candidates-length
         (stringp company-prefix)
         (window-live-p win)
         (pos-visible-in-window-p
          (if (eq company-box-frame-behavior 'point)
              (point)
            (max (point-min) (- (point) (length company-prefix))))
          win))))
