;; -*- lexical-binding: t; -*-
;;; lx-fix-ambiguous-char-width.el --- 修正 East Asian Ambiguous 符号宽度，消除表格竖线错位
;;
;; 背景与根因：
;;   等宽西文字体（如 JetBrainsMono）含有 —、→、÷、≠ 等「East Asian Ambiguous
;;   Width」字符的字形并按半角（1 列）渲染。但在 CJK 语境下 Emacs 的 `char-width'
;;   对这些字符返回 2。这种「逻辑列宽 2 / 实际像素 1 列」的不一致会导致
;;   markdown / org 表格竖线（按像素拉伸）错位。
;;
;;   其中 ÷、× 等字符属于 Emacs 的 `symbol' charset，而 `lx/set-monospaced-font'
;;   把 `symbol' charset 显式回退到了中文字体（黑体）。黑体对 ÷、× 给的是非整数
;;   列宽（÷=12px、×=11px），即使改 `char-width' 也无法对齐。但 JetBrainsMono 本身
;;   就有这两个字符的等宽字形（8px），所以需要先用 `set-fontset-font' 把它们「抢
;;   回」default 字体渲染，再把 `char-width' 设为 1，两步配合才能完美对齐。
;;
;; 两步修正：
;;   1. fontset 单字符级 prepend：把被 `symbol' charset 错误回退到中文字体、
;;      且 default 字体有等宽字形的字符，指回 default 字体（8px 半角渲染）。
;;   2. char-width 修正：把上述半角渲染字符的 `char-width' 设为 1，使逻辑列宽
;;      与像素一致，`string-width' 计算准确，表格对齐算法正确 padding。
;;
;; 注意：
;;   - 汉字、全角标点（（ ％ 等）不受影响，仍是中文字体 2 列。
;;   - 本函数需在 `lx/set-monospaced-font' *之后* 调用（需要 default 字体已设定）。
;;   - 可安全重复执行（幂等）。

;;; Code:

;; 这些字符 East Asian Width = Ambiguous，default 字体（JetBrainsMono）按半角 8px
;; 渲染，但 `char-width' 默认为 2。统一设 char-width 为 1 使逻辑列宽与像素一致。
;; 清单覆盖通用标点、数学运算符、箭头、Latin-1 符号、杂项符号等表格/文档常见者。
(defconst lx/ambiguous-narrow-chars
  (list
   ;; 通用标点
   ?– ?— ?‖ ?‘ ?’ ?“ ?” ?‥ ?… ?‰ ?′ ?″ ?‴ ?※ ?‾ ?⁁ ?⁂ ?⁃
   ;; 箭头
   ?← ?↑ ?→ ?↓ ?↔ ?↕ ?↖ ?↗ ?↘ ?↙ ?⇐ ?⇑ ?⇒ ?⇓ ?⇔ ?⇕
   ;; 数学运算符
   ?≠ ?≤ ?≥ ?± ?∓ ?× ?÷ ?∈ ?∉ ?∋ ?∏ ?∑ ?− ?√ ?∝ ?∞ ?∠ ?∧ ?∨ ?∩ ?∪ ?∫ ?∴ ?∵ ?≈ ?≡ ?⊂ ?⊃ ?⊆ ?⊇ ?⊕ ?⊖ ?⊗ ?⊙
   ;; Latin-1 符号（等宽字体半角渲染的）
   ?° ?· ?• ?« ?» ?¿ ?¡ ?¢ ?£ ?¤ ?¥ ?¦ ?§ ?¨ ?© ?ª ?¬ ?® ?¯ ?² ?³ ?´ ?µ ?¶ ?¸ ?¹ ?º ?¼ ?½ ?¾
   ;; 杂项符号
   ?™ ?℃ ?℉ ?♯ ?♭ ?♪ ?♮ ?✓ ?✔ ?✗ ?✘ ?★ ?☆ ?◆ ?◇ ?○ ?● ?□ ?■ ?△ ?▲ ?▽ ?▼)
  "East Asian Ambiguous Width 字符列表，`char-width' 设为 1（半角）。")

;; 这些字符同时属于 Emacs 的 `symbol' charset（会被 `lx/set-monospaced-font'
;; 回退到中文字体），但 default 等宽字体本身有它们的字形（等宽 8px）。用
;; `set-fontset-font' 单字符级 prepend 把它们指回 default 字体渲染，避免中文字体
;; 给出非整数列宽（如黑体的 ÷=12px）。仅列需要「抢回」的字符；其它 ambiguous
;; 字符（—、→ 等）本就不在 `symbol' charset，由 default 字体直接渲染。
(defconst lx/ambiguous-pin-to-default-chars
  '((#x00D7 . #x00D7)   ; × U+00D7
    (#x00F7 . #x00F7)   ; ÷ U+00F7
    (#x2190 . #x21FF)   ; 箭头块 Arrows（→ ← ⇒ ⇔ 等，属 symbol charset）
    (#x2200 . #x22FF)   ; 数学运算符 Mathematical Operators（≠ ≤ ≥ ∈ ∞ √ 等）
    (#x2300 . #x23FF)   ; 杂项技术 Miscellaneous Technical
    (#x25A0 . #x25FF)   ; 几何图形 Geometric Shapes（◆ □ ▲ 等）
    (#x2600 . #x26FF))  ; 杂项符号 Misc Symbols（★ ☆ ☀ 等）
  "需从 `symbol' charset 黑体回退中「抢回」default 字体的字符码点范围。
仅当 default 字体确实含这些字符字形时有效（JetBrainsMono 覆盖良好）。")

;;;###autoload
(defun lx/fix-ambiguous-char-width ()
  "修正 East Asian Ambiguous 符号宽度，消除 markdown/org 表格竖线错位。
两步：(1) 把被 `symbol' charset 错误回退的字符指回 default 字体；(2) 把半角
渲染字符的 `char-width' 设为 1。需在 `lx/set-monospaced-font' 之后调用。"
  (interactive)
  ;; 步骤 1：fontset 单字符修正——把 ÷ × 等指回 default 字体。
  ;; 取 default face 字体的 family；`frame-parameter font' 是 XLFD，转 family。
  (let* ((frame (selected-frame))
         (default-family (face-attribute 'default :family frame)))
    (when (and (stringp default-family) (not (string-empty-p default-family)))
      (let ((pin-spec (font-spec :family default-family)))
        (dolist (range lx/ambiguous-pin-to-default-chars)
          (dolist (fs (list (frame-parameter frame 'font) t))
            (ignore-errors
              (set-fontset-font fs range pin-spec frame 'prepend)))))))
  ;; 步骤 2：char-width 修正——半角渲染字符设为 1 列。
  (when (char-table-p char-width-table)
    (dolist (c lx/ambiguous-narrow-chars)
      (set-char-table-range char-width-table c 1)))
  (when (called-interactively-p 'interactive)
    (message "Ambiguous 符号宽度已修正（%d 个字符 char-width=1 + fontset 抢回 %d 范围）"
             (length lx/ambiguous-narrow-chars)
             (length lx/ambiguous-pin-to-default-chars))))

(provide 'lx-fix-ambiguous-char-width)
;;; lx-fix-ambiguous-char-width.el ends here
