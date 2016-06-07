(defun lx/browse-url-in-safari (url)
  (interactive)
  (call-process-shell-command (format "echo 'tell app \"Safari\"
tell window 1
set current tab to (make new tab with properties {URL:\"%s\"})
end tell
activate
end tell' | osascript -ss
" url)))

