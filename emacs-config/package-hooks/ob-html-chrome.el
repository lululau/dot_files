(with-eval-after-load 'ob-html-chrome
  (defun org-babel-execute:html-chrome (body params)
    "Render the HTML in BODY using PARAMS."
    (unless (f-executable? org-babel-html-chrome-chrome-executable)
      (error "Can not export HTML: `%s' (specified by org-babel-html-chrome-chrome-executable) does not exist or is not executable" org-babel-html-chrome-chrome-executable))
    (let* ((processed-params (org-babel-process-params params))
    (org-babel-temporary-directory default-directory)
    (html-file (org-babel-temp-file "ob-html-chrome" ".html"))
    (url
      (or (cdr (assoc :url processed-params))
          (concat "file://" (org-babel-process-file-name html-file))))
    (out-file
      (or (cdr (assoc :file processed-params)) ; :file arg
          (nth 4 (org-babel-get-src-block-info)) ; #+NAME of block
          (s-dashed-words (nth 4 (org-heading-components))))) ; Heading
    (flags (cdr (assoc :flags processed-params)))
    (_size (cdr (assoc :size processed-params)))
    (size (and _size (format "--window-size=%s" _size)))
    (_scale (cdr (assoc :scale processed-params)))
    (scale (and _scale (format "--force-device-scale-factor=%s" _scale)))
    (cmd (s-join
          " "
          `(,(shell-quote-argument
        org-babel-html-chrome-chrome-executable)
      ,@'("--headless" "--disable-gpu" "--enable-logging")
      ,flags
      ,size
      ,scale
      ,(format "--screenshot=%s"
          (org-babel-process-file-name out-file))
      ,url))))
      (with-temp-file html-file
        (insert body))
      (org-babel-eval cmd "")
      (delete-file html-file)
      nil)))

