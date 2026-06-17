;; -*- lexical-binding: t; -*-
;;; lx-fix-ambiguous-char-width.el --- 把半角渲染的 East Asian Ambiguous 符号 char-width 设为 1
;;
;; 背景：JetBrainsMono 等等宽西文字体本身含有 —、→、÷、≠ 等「East Asian
;; Ambiguous Width」字符的字形，并按半角（1 列）渲染。但在 CJK 语境下，
;; Emacs 的 `char-width' 对这些字符返回 2。这种「逻辑列宽 2 / 实际像素 1 列」
;; 的不一致会导致 markdown / org 表格竖线（按像素拉伸）错位。
;;
;; 由于 default 字体已含这些字符的字形，`set-fontset-font' 无法覆盖（fontset
;; 规则仅在 default 字体缺失字形时回退）。因此改走另一条路：把这些符号的
;; `char-width' 设为 1，使其逻辑列宽与半角像素渲染一致，于是 `string-width'
;; 计算准确，表格对齐算法（`markdown-table-align' 等）能正确 padding。
;;
;; 注意：
;;   - 汉字、全角标点（（）％Σ 等）不受影响，仍是 2 列。
;;   - `char-width-table' 是全局 char-table，Emacs 在创建新 frame/重设字体
;;     时可能重建它，故本函数需在字体设置 *之后* 调用，且可安全重复执行。

;;; Code:

;; 这些字符 East Asian Width = Ambiguous，char-width 默认为 2，但等宽西文字体
;; （JetBrainsMono Nerd Font）按半角渲染。逐一设为 1 使逻辑列宽与像素一致。
;; 清单覆盖通用标点、数学运算符、箭头、Latin-1 符号等表格/文档中最常见者。
(defconst lx/ambiguous-narrow-chars
  (list
   ;; 通用标点
   ?– ?— ?‖ ?‘ ?’ ?“ ?” ?‥ ?… ?‰ ?′ ?″ ?‴ ?※ ?‾ ?⁁ ?⁂ ?⁃
   ;; 箭头
   ?← ?↑ ?→ ?↓ ?↔ ?↕ ?↖ ?↗ ?↘ ?↙ ?⇐ ?⇑ ?⇒ ?⇓ ?⇔ ?⇕
   ;; 数学运算符
   ?≠ ?≤ ?≥ ?± ?∓ ?× ?÷ ?∈ ?∉ ?∋ ?∏ ?∑ ?− ?√ ?∝ ?∞ ?∠ ?∧ ?∨ ?∩ ?∪ ?∫ ?∴ ?∵ ?≈ ?≡ ?≡ ?⊂ ?⊃ ?⊆ ?⊇ ?⊕ ?⊖ ?⊗ ?⊙
   ;; Latin-1 符号（等宽字体半角渲染的）
   ?° ?· ?• ?· ?« ?» ?¿ ?¡ ?¢ ?£ ?¤ ?¥ ?¦ ?§ ?¨ ?© ?ª ?« ?¬ ?® ?¯ ?° ?± ?² ?³ ?´ ?µ ?¶ ?· ?¸ ?¹ ?º ?» ?¼ ?½ ?¾
   ;; 杂项符号
   ?™ ?© ?® ?℃ ?℉ ?♯ ?♭ ?♪ ?♮ ?✓ ?✔ ?✗ ?✘ ?★ ?☆ ?◆ ?◇ ?○ ?● ?□ ?■ ?△ ?▲ ?▽ ?▼)
  "East Asian Ambiguous Width 字符列表，char-width 设为 1（半角）。")

;;;###autoload
(defun lx/fix-ambiguous-char-width ()
  "把半角渲染的 Ambiguous 符号的 `char-width' 设为 1。
在 `lx/set-monospaced-font' 之后调用，让逻辑列宽与实际像素一致，
修正 markdown/org 表格竖线错位。可安全重复执行（幂等）。"
  (interactive)
  (when (char-table-p char-width-table)
    (dolist (c lx/ambiguous-narrow-chars)
      (set-char-table-range char-width-table c 1)))
  (when (called-interactively-p 'interactive)
    (message "Ambiguous char-width 已修正（%d 个符号设为 1 列）"
             (length lx/ambiguous-narrow-chars))))

(provide 'lx-fix-ambiguous-char-width)
;;; lx-fix-ambiguous-char-width.el ends here
