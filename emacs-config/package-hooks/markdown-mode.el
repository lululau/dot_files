;; -*- lexical-binding: t; -*-

(with-eval-after-load 'markdown-mode
  ;; Extend code block background to window right edge
  (set-face-attribute 'markdown-code-face nil :extend t)

  ;; org-modern style box-drawn tables for markdown
  (add-hook 'markdown-mode-hook #'markdown-modern-mode)

  ;; 修正 markdown 表格对齐时 FE0F（Variation Selector 16）的宽度计算。
  ;;
  ;; 问题链：
  ;;   1. `markdown--table-line-to-columns' 用 `buffer-substring-no-properties'
  ;;      提取 cell 内容，丢失 emoji composition 属性。
  ;;   2. `markdown--string-width' → `string-width' 在无 composition 的字符串上
  ;;      对 ⚠+FE0F 返回 1（⚠=1, FE0F=0），但实际渲染为 2 列宽 emoji。
  ;;   3. `markdown-table-align-raw' 用 `format %Ns' 做 padding，后者调用 C 层
  ;;      `string-width'（不可 advice），同样算出 1 列，导致 over-padding。
  ;;
  ;; 两步修复：
  ;;   (a) advice `markdown--string-width' 补偿 FE0F；
  ;;   (b) advice `markdown-table-align-raw' 用手动 padding 替代 `format %Ns'，
  ;;       确保 padding 使用修正后的 `markdown--string-width'。

  (defun lx/markdown--string-width-fix-fe0f (orig-fn s)
    "补偿 FE0F (VS16) 使前导字符以 emoji 形式宽渲染但 string-width 未计入的差异。"
    (let ((result (funcall orig-fn s))
          (i 0)
          (len (length s)))
      (while (< i len)
        (when (and (= (aref s i) #xFE0F)
                   (> i 0)
                   (= (char-width (aref s (1- i))) 1))
          (cl-incf result))
        (cl-incf i))
      result))
  (advice-add 'markdown--string-width :around #'lx/markdown--string-width-fix-fe0f)

  (defun lx/markdown-table-align-raw (orig-fn cells fmtspec widths)
    "用 `markdown--string-width' 手动 padding，替代 `format %Ns' 的内置 string-width。"
    (ignore orig-fn)
    (let (fmt width)
      (mapconcat
       (lambda (cell)
         (setq fmt (car fmtspec) fmtspec (cdr fmtspec))
         (setq width (car widths) widths (cdr widths))
         (let* ((cell-w (markdown--string-width cell))
                (pad (max 0 (- width cell-w))))
           (cond
            ((equal fmt 'c)
             (let ((lpad (/ pad 2))
                   (rpad (- pad (/ pad 2))))
               (concat " " (make-string lpad ?\s) cell (make-string rpad ?\s) " ")))
            ((equal fmt 'r)
             (concat " " (make-string pad ?\s) cell " "))
            (t
             (concat " " cell (make-string pad ?\s) " ")))))
       cells "|")))
  (advice-add 'markdown-table-align-raw :around #'lx/markdown-table-align-raw)

  (defun lx/markdown-align-all-tables ()
    "对齐当前 Markdown Buffer 中的所有表格。"
    (interactive)
    (unless (derived-mode-p 'markdown-mode)
      (user-error "当前 Buffer 不是 markdown-mode"))
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (markdown-table-at-point-p)
              (let ((table-end (markdown-table-end)))
                (markdown-table-align)
                (cl-incf count)
                (goto-char (or table-end (line-end-position)))
                (unless (eobp)
                  (forward-line 1)))
            (forward-line 1))))
      (message "已成功对齐当前 Buffer 中的 %d 个表格。" count)))

  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "'" 'markdown-edit-code-block)
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "ta" #'lx/markdown-align-all-tables)
  (evil-define-key 'motion markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (evil-define-key 'normal markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "C-i") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "M-h") nil)
  (define-key markdown-mode-map (kbd "M-j") nil)
  (define-key markdown-mode-map (kbd "M-k") nil)
  (define-key markdown-mode-map (kbd "M-l") nil)
  (define-key markdown-mode-map (kbd "M-H") nil)
  (define-key markdown-mode-map (kbd "M-J") nil)
  (define-key markdown-mode-map (kbd "M-K") nil)
  (define-key markdown-mode-map (kbd "M-L") nil)
  (define-key markdown-mode-map (kbd "S-<tab>") 'markdown-shifttab)
  ;; (add-hook 'markdown-mode-hook #'turn-company-english-helper-on 100)

  (defun markdown-before-first-heading-p ()
    "Return non-nil if point is before the first heading."
    (save-excursion
      (let ((pos (point)))
        (goto-char (point-min))
        (let ((first-heading (markdown-next-heading)))
          (or (not first-heading) (< pos first-heading))))))

  (defun markdown-show-branches-buffer ()
    "Show all branches in the buffer."
    (outline-hide-sublevels 1)
    (save-excursion
      (goto-char (point-min))
      (when (markdown-next-heading)
        (outline-show-branches)
        (while (outline-get-next-sibling)
          (outline-show-branches))))
    (goto-char (point-min))
    (markdown-outline-fix-visibility))

  (defun markdown-show-branches ()
    "Hide subtree body but show child headings, like `org-kill-note-or-show-branches'.
When before the first heading, show all branches in the buffer."
    (interactive)
    (cond
     ((markdown-before-first-heading-p)
      (markdown-show-branches-buffer)
      (message "BRANCHES"))
     (t
      (markdown-back-to-heading)
      (outline-hide-subtree)
      (outline-show-children 1000)
      (setq markdown-cycle-subtree-status 'children)
      (message "CHILDREN"))))

  (define-key markdown-mode-map (kbd "C-c C-k") #'markdown-show-branches)
  (define-key markdown-mode-map [remap outline-show-branches] #'markdown-show-branches)

  (defun markdown-cycle (&optional arg)
    "Visibility cycling for Markdown mode.
  This function is called with a `\\[universal-argument]' or if ARG is t, perform
  global visibility cycling.  If the point is at an atx-style header, cycle
  visibility of the corresponding subtree.  Otherwise, indent the current line
  or insert a tab, as appropriate, by calling `indent-for-tab-command'."
    (interactive "P")
    (cond

    ;; Global cycling
    (arg
      (cond
      ;; Move from overview to contents
      ((and (eq last-command this-command)
            (eq markdown-cycle-global-status 2))
        (outline-hide-sublevels 1)
        (message "CONTENTS")
        (setq markdown-cycle-global-status 3)
        (markdown-outline-fix-visibility))
      ;; Move from contents to all
      ((and (eq last-command this-command)
            (eq markdown-cycle-global-status 3))
        (outline-show-all)
        (message "SHOW ALL")
        (setq markdown-cycle-global-status 1))
      ;; Defaults to overview
      (t
        (outline-hide-body)
        (message "OVERVIEW")
        (setq markdown-cycle-global-status 2)
        (markdown-outline-fix-visibility))))

    ;; At a heading: rotate between three different views
    ((save-excursion (beginning-of-line 1) (markdown-on-heading-p))
      (markdown-back-to-heading)
      (let ((goal-column 0) eoh eol eos)
        ;; Determine boundaries
        (save-excursion
          (markdown-back-to-heading)
          (save-excursion
            (beginning-of-line 2)
            (while (and (not (eobp)) ;; this is like `next-line'
                        (get-char-property (1- (point)) 'invisible))
              (beginning-of-line 2)) (setq eol (point)))
          (markdown-end-of-heading)   (setq eoh (point))
          (markdown-end-of-subtree t)
          (skip-chars-forward " \t\n")
          (beginning-of-line 1) ; in case this is an item
          (setq eos (1- (point))))
        ;; Find out what to do next and set `this-command'
        (cond
        ;; Nothing is hidden behind this heading
        ((= eos eoh)
          (message "EMPTY ENTRY")
          (setq markdown-cycle-subtree-status nil))
        ;; Entire subtree is hidden in one line: open it
        ((>= eol eos)
          (markdown-show-entry)
          (outline-show-children)
          (message "CHILDREN")
          (setq markdown-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        ((and (eq last-command this-command)
              (eq markdown-cycle-subtree-status 'children))
          (outline-show-subtree)
          (message "SUBTREE")
          (setq markdown-cycle-subtree-status 'subtree))
        ;; Default action: hide the subtree.
        (t
          (outline-hide-subtree)
          (message "FOLDED")
          (setq markdown-cycle-subtree-status 'folded)))))

    ;; In a table, move forward by one cell
    ((markdown-table-at-point-p)
      (call-interactively #'markdown-table-forward-cell))

    ;; Otherwise, indent as appropriate
    (t
      (call-interactively #'lx/tab))))

  )
