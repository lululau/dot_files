
;;;###autoload
(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

;; (defun gptel-mcp-use-tool ()
;;   (interactive)
;;   (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
;;     (mapcar #'(lambda (tool)
;;                 (let ((path (list (plist-get tool :category)
;;                                   (plist-get tool :name))))
;;                   (push (gptel-get-tool path)
;;                         gptel-tools)))
;;             tools)))

;;;###autoload
(defun mcp-hub-start (&optional callback)
  (interactive)
  (unless mcp-started
      (mcp-hub-start-all-server callback)
    ;; (gptel-mcp-register-tool)
    ;; (gptel-mcp-use-tool)
    (setq mcp-started t))
  (call-interactively 'mcp-hub))
