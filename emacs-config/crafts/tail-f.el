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


(defun tail-f--start-process (file lines string-buf-var)
  (let* ((name (format "* %s*" string-buf-var)))
    (set string-buf-var '())
    (process-put (make-process :name name
                  :command (list "tail" "-n" (number-to-string lines) "-f" file)
                  :coding 'utf-8
                  :connection-type 'pipe
                  :filter 'tail-f--process-filter
                  :noquery t)
                 'tail-f-string-buf string-buf-var)))

(defun tail-f (file lines)
  (let* ((filename (expand-file-name file))
         (string-buf-var (tail-f--get-string-buf-var filename)))
    (if (boundp string-buf-var)
        (if (eval string-buf-var)
            (eval string-buf-var)
          (process-lines "tail" "-n" (number-to-string lines) filename))
      (tail-f--start-process filename lines string-buf-var)
      (process-lines "tail" "-n" (number-to-string lines) filename))))

(provide 'tail-f)
