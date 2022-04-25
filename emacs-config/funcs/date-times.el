(defun lx/insert-timestamp ()
  "Read a timestamp from the user and insert it at point."
  (interactive)
  (let* ((time (current-time))
        (insert-func (plist-get (symbol-plist major-mode) 'insert-function))
        (insert-func (or insert-func 'insert)))
    (helm :prompt "Timestamp: "
          :buffer "*Helm Timestamp*"
          :sources
          `(((name . "Dates")
             (candidates . ,(list
                             (format-time-string "%Y-%m-%d %H:%M:%S" time)
                             (format-time-string "%Y-%m-%d" time)
                             (format-time-string "%Y-%m-%d %I:%M:%S %p" time)))
             (action . ,insert-func)
             (volatile))

            ((name . "Times")
             (candidates . ,(list
                             (format-time-string "%X" time)
                             (format-time-string "%H:%M:%S" time)
                             (format-time-string "%I:%M:%S %p" time)))
             (action . ,insert-func)
             (volatile))

            ((name . "Special")
             (candidates . ,(list
                             (format "%d" (float-time time))
                             (format-time-string "%d %B, %Y" time)
                             (format-time-string "%Y-%m-%dT%H%M%S%z")))
             (action . ,insert-func)
             (volatile))))))

(defun lx/parse-timestamp ()
  (interactive)
  (let* ((time-string (thing-at-point 'word))
         (seconds (string-to-number time-string))
         (time (seconds-to-time seconds))
         (readable-string (format-time-string "%Y-%m-%d %H:%M:%S" time)))
    (kill-new time-string)
    (kill-new readable-string)
    (message readable-string)))
