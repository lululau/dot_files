(require 'cl-lib)
(require 's)
(require 'dash)

(defvar tail-f--id 0)
(defvar tail-f--name-id-mapping '())

(defun tail-f--process-filter (process output)
  (let* ((string-buf-var (process-get process 'tail-f-string-buf))
         (lines (s-split "\n" output t)))
    (set string-buf-var (append (eval string-buf-var) lines))))

(defun tail-f--get-string-buf-var (filename)
  (let* ((buf-id (alist-get filename tail-f--name-id-mapping nil nil 'string=)))
    (when (not buf-id)
      (setq buf-id (cl-incf tail-f--id))
      (add-to-list 'tail-f--name-id-mapping (cons filename buf-id)))
    (let* ((string-buf-name (format "tail-f--string-buf-%d" buf-id))
          (string-buf-symbol (intern string-buf-name)))
      string-buf-symbol)))


(defun tail-f--start-process (command string-buf-var)
  (let* ((name (format "* %s*" string-buf-var)))
    (set string-buf-var '())
    (process-put (make-process :name name
                  :command (list "bash" "-c" command)
                  :coding 'utf-8
                  :connection-type 'pipe
                  :filter 'tail-f--process-filter
                  :noquery t)
                 'tail-f-string-buf string-buf-var)))

(defun tail-f (file lines &optional command)
  (let* ((filename (shell-quote-argument (expand-file-name file)))
         (string-buf-var (tail-f--get-string-buf-var filename)))
    (if (not command)
        (setq command "tail -n %d %s -f"))
    (setq command (format command lines filename))
    (if (boundp string-buf-var)
        (if (eval string-buf-var)
            (eval string-buf-var)
          ;; TODO replace with a better solution
          (process-lines "bash" "-c" (replace-regexp-in-string "-f" "" command)))
      (tail-f--start-process command string-buf-var)
      (process-lines "bash" "-c" (replace-regexp-in-string "-f" "" command)))))

(provide 'tail-f)
