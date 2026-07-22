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
;;   2. char-width 动态修正：用 `string-pixel-width' 测量每个字符的实际渲染
;;      像素宽度，根据 frame-char-width 计算真实列数，设定 `char-width'。
;;      这样即使字符 fallback 到 Heiti SC 等 CJK 字体（全角渲染），也能正确
;;      设为 2 列，而非盲目设为 1。
;;
;; 注意：
;;   - 汉字、全角标点（（ ％ 等）不受影响，仍是中文字体 2 列。
;;   - 本函数需在 `lx/set-monospaced-font' *之后* 调用（需要 default 字体已设定）。
;;   - 可安全重复执行（幂等）。

;;; Code:

;; 候选字符列表——East Asian Width = Ambiguous 或常见于表格/文档中的特殊符号。
;; 覆盖通用标点、数学运算符、箭头、Latin-1 符号、杂项符号。
;; `lx/fix-ambiguous-char-width' 会动态测量每个字符的实际像素宽度来决定 char-width，
;; 而非盲目全设为 1——部分字符可能 fallback 到 CJK 字体并以全角渲染。
(defconst lx/ambiguous-candidate-chars
  (list
   ;; 通用标点
   ?– ?— ?‖ ?' ?' ?" ?" ?‥ ?… ?‰ ?′ ?″ ?‴ ?※ ?‾ ?⁁ ?⁂ ?⁃
   ;; 箭头
   ?← ?↑ ?→ ?↓ ?↔ ?↕ ?↖ ?↗ ?↘ ?↙ ?⇐ ?⇑ ?⇒ ?⇓ ?⇔ ?⇕
   ;; 数学运算符
   ?≠ ?≤ ?≥ ?± ?∓ ?× ?÷ ?∈ ?∉ ?∋ ?∏ ?∑ ?− ?√ ?∝ ?∞ ?∠ ?∧ ?∨ ?∩ ?∪ ?∫ ?∴ ?∵ ?≈ ?≡ ?⊂ ?⊃ ?⊆ ?⊇ ?⊕ ?⊖ ?⊗ ?⊙
   ;; Latin-1 符号
   ?° ?· ?• ?« ?» ?¿ ?¡ ?¢ ?£ ?¤ ?¥ ?¦ ?§ ?¨ ?© ?ª ?¬ ?® ?¯ ?² ?³ ?´ ?µ ?¶ ?¸ ?¹ ?º ?¼ ?½ ?¾
   ;; 杂项符号
   ?™ ?℃ ?℉ ?♯ ?♭ ?♪ ?♮ ?✓ ?✔ ?✗ ?✘ ?★ ?☆ ?◆ ?◇ ?○ ?● ?□ ?■ ?△ ?▲ ?▽ ?▼
   ;; 常以 +FE0F emoji 形式出现的基础字符（char-width 取 emoji 渲染宽度）
   ?⚠ ?❤ ?☺ ?☹ ?☎ ?✉ ?☀ ?☁ ?☂ ?⚡ ?⚽ ?⚾ ?☕)
  "East Asian Ambiguous Width 候选字符列表。
实际 `char-width' 值由 `lx/fix-ambiguous-char-width' 根据像素测量动态决定。")

;; 向后兼容：旧名别名
(defvaralias 'lx/ambiguous-narrow-chars 'lx/ambiguous-candidate-chars)

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
    (#x25A0 . #x25FF))  ; 几何图形 Geometric Shapes（◆ □ ▲ 等）
  "需从 `symbol' charset 黑体回退中「抢回」default 字体的字符码点范围。
仅当 default 字体确实含这些字符字形时有效（JetBrainsMono 覆盖良好）。
注意：U+2600-U+26FF（Misc Symbols，含★ ☆）已移除——JetBrainsMono 未必含这些字形，
fallback 到 Heiti SC 全角渲染时强行 prepend 会导致缺字或宽度错误。")

(defun lx/--measure-char-columns (char)
  "测量 CHAR 的实际渲染像素宽度，返回最接近的列数（1 或 2）。
利用 `string-pixel-width' 与 `frame-char-width'（单列宽度）比较。
同时检查 CHAR 附加 FE0F（Variation Selector 16）后的像素宽度——
许多字符（如 ⚠ ❤ ☺）在实际文档中几乎总是以 emoji 形式（+FE0F）出现，
渲染宽度显著大于独立形式。取两者中更大的宽度来决定 char-width。
像素宽度 <= 1.5 列视为 1 列，否则为 2 列。"
  (let* ((space-w (frame-char-width))
         (px-standalone (string-pixel-width (string char)))
         (px-with-fe0f (string-pixel-width (string char #xFE0F)))
         (px (max px-standalone px-with-fe0f))
         (threshold (+ space-w (/ space-w 2))))  ; 1.5 × space-w
    (if (<= px threshold) 1 2)))

;;;###autoload
(defun lx/fix-ambiguous-char-width ()
  "修正 East Asian Ambiguous 符号宽度，消除 markdown/org 表格竖线错位。
两步：(1) 把被 `symbol' charset 错误回退的字符指回 default 字体；
(2) 动态测量每个候选字符的实际像素宽度，将 `char-width' 设为真实列数。
需在 `lx/set-monospaced-font' 之后调用。"
  (interactive)
  ;; 步骤 1：fontset 单字符修正——把 ÷ × 等指回 default 字体。
  (let* ((frame (selected-frame))
         (default-family (face-attribute 'default :family frame)))
    (when (and (stringp default-family) (not (string-empty-p default-family)))
      (let ((pin-spec (font-spec :family default-family)))
        (dolist (range lx/ambiguous-pin-to-default-chars)
          (dolist (fs (list (frame-parameter frame 'font) t))
            (ignore-errors
              (set-fontset-font fs range pin-spec frame 'prepend)))))))
  ;; 步骤 2：动态测量像素宽度并设置 char-width。
  ;; 先 fontset 抢回后再测量，因为抢回可能改变字符的渲染字体和像素宽度。
  (when (and (char-table-p char-width-table)
             (fboundp 'string-pixel-width))
    (let ((fixed-1 0) (fixed-2 0))
      (dolist (c lx/ambiguous-candidate-chars)
        (let* ((actual-cols (lx/--measure-char-columns c))
               (current-cw (char-width c)))
          (unless (= current-cw actual-cols)
            (set-char-table-range char-width-table c actual-cols)
            (if (= actual-cols 1) (cl-incf fixed-1) (cl-incf fixed-2)))))
      (when (called-interactively-p 'interactive)
        (message "Ambiguous 符号宽度已修正（%d→1列, %d→2列, fontset 抢回 %d 范围）"
                 fixed-1 fixed-2
                 (length lx/ambiguous-pin-to-default-chars))))))

(provide 'lx-fix-ambiguous-char-width)
;;; lx-fix-ambiguous-char-width.el ends here
