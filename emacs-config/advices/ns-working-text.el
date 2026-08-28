;; -*- lexical-binding: t; -*-
;; 规避 macOS NS 端口 IME bug（Emacs 31.1 lisp/term/ns-win.el）：
;; minibuffer（helm/isearch）中系统输入法的组合文本走 echo-area 路径，
;; ns-echo-working-text 把 ns-working-overlay 存为整数（组合串长度）；
;; helm/异步进程清空 echo area 后，ns-delete-working-text 执行
;; (substring (current-message) …) 遇到 nil 报 "Wrong type argument:
;; arrayp, nil"，错误向上打断 IME 事件处理，导致中文无法上屏。
;; echo 区已无可剥离的消息时，直接清掉残留整数状态，不再调原实现。

(when (and (eq system-type 'darwin)
           (fboundp 'ns-delete-working-text))
  (advice-add 'ns-delete-working-text :around
              (lambda (orig)
                (if (and (integerp ns-working-overlay)
                         (or (null (current-message))
                             (< (length (current-message))
                                ns-working-overlay)))
                    (setq ns-working-overlay nil)
                  (funcall orig)))))
