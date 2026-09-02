;; -*- lexical-binding: t; -*-

;; .json5 文件不需要 LSP：lsp-mode 的 json-ls（vscode-json-language-server）
;; 只能解析严格 JSON/JSONC，会对合法 JSON5（无引号 key、尾逗号等）报
;; "Property keys must be doublequoted"。json layer 在 json-mode-hook 里
;; 无条件 (lsp-deferred)，而 json5-mode 入口运行的就是 json-mode，hook 照常
;; 触发，所以这里对 .json5 文件直接跳过该 hook，避免无意义的 server 启动
;; 和 "No LSP server" 提示。json5-mode 包本身已在 buffer 里设置
;; lsp-disabled-clients 兜底（见 lululau/json5-mode 的 json5-mode 函数）。

(defun lx/json-setup-backend-skip-json5 (orig-fun)
  "Call ORIG-FUN unless the current buffer is a .json5 file."
  (unless (and buffer-file-name
               (equal (file-name-extension buffer-file-name) "json5"))
    (funcall orig-fun)))

(advice-add 'spacemacs//json-setup-backend :around #'lx/json-setup-backend-skip-json5)
