;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'run-in-ghostel)
(require 'json)

(defconst ghostel-agent--new-session-label "New Session")

(cl-defstruct (ghostel-agent-session
               (:constructor ghostel-agent-session-create)
               (:conc-name ghostel-agent-session-))
  "Agent session metadata for Ghostel resume picker."
  id title mtime updated)

(defun ghostel-agent--agent-buffer-name-p (name)
  "Return non-nil if NAME looks like an agent Ghostel buffer from `ghostel-agent-run-*`."
  (string-match-p "\\`\\*ghostel-\\(?:claude\\|opencode\\|cursor\\|agy\\|aliyun-tp-claude\\)\\[" name))

(defun ghostel-agent--agent-buffer-p (buffer)
  "Return non-nil if BUFFER is a live Ghostel agent shell."
  (and (buffer-live-p buffer)
       (ghostel-agent--agent-buffer-name-p (buffer-name buffer))
       (with-current-buffer buffer (derived-mode-p 'ghostel-mode))))

(defun ghostel-agent--first-visible-agent-buffer ()
  "Return the first Ghostel agent buffer displayed in some window on a visible frame.

Walk `(frame-list)' order; for each frame use `(window-list FRAME nomini)',
so minibuffer-only windows are skipped.

Do not use `(window-list nil nomini t)': third argument is FRAME on some Emacs
releases, passing `windowp t'-style errors when given `t'."
  (catch 'ghostel-agent--found
    (dolist (frm (frame-list))
      (when (frame-visible-p frm)
        (dolist (win (window-list frm 'nomini))
          (let ((buf (window-buffer win)))
            (when (ghostel-agent--agent-buffer-p buf)
              (throw 'ghostel-agent--found buf))))))))

(defun ghostel-agent--project-root ()
  "Return the current Ghostel agent project root."
  (lx/run-in-projectile-ghostel--scope-root))

(defun ghostel-agent--directory-in-project-p (path project-root)
  "Return non-nil when PATH belongs to PROJECT-ROOT."
  (and path project-root
       (string-prefix-p (file-name-as-directory (expand-file-name project-root))
                        (file-name-as-directory (expand-file-name path)))))

(defun ghostel-agent--json-parse-line (line)
  "Parse JSON object from LINE, returning nil on failure."
  (when (and (stringp line) (not (string-empty-p (string-trim line))))
    (condition-case nil
        (json-parse-string line :object-type 'alist)
      (error nil))))

(defun ghostel-agent--jsonl-each (file fn)
  "Call FN with each parsed JSON object from FILE."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point) (line-end-position))))
          (forward-line 1)
          (when-let* ((obj (ghostel-agent--json-parse-line line)))
            (funcall fn obj)))))))

(defun ghostel-agent--truncate-title (title &optional max-len)
  "Return TITLE truncated to MAX-LEN (default 50) characters."
  (let ((title (or title "")))
    (if (> (length title) (or max-len 50))
        (concat (substring title 0 (- (or max-len 50) 3)) "...")
      title)))

(defun ghostel-agent--clean-display-text (text)
  "Normalize TEXT for session picker titles."
  (let ((text (or text "")))
    (setq text (replace-regexp-in-string "<[^>]+>" "" text))
    (setq text (replace-regexp-in-string "[ \t\n\r]+" " " text))
    (string-trim text)))

(defun ghostel-agent--format-session-date (timestamp)
  "Format TIMESTAMP for session picker display.

TIMESTAMP may be a float seconds value, an ISO-8601 string, or nil."
  (condition-case nil
      (let* ((time (cond
                    ((numberp timestamp) timestamp)
                    ((stringp timestamp) (date-to-time timestamp))
                    (t (error "unsupported timestamp"))))
             (now (current-time))
             (decoded-now (decode-time now))
             (today-start (encode-time 0 0 0
                                       (decoded-time-day decoded-now)
                                       (decoded-time-month decoded-now)
                                       (decoded-time-year decoded-now)))
             (yesterday-start (time-subtract today-start (seconds-to-time (* 24 60 60))))
             (current-year (decoded-time-year (decode-time now)))
             (timestamp-year (decoded-time-year (decode-time time))))
        (cond
         ((not (time-less-p time today-start))
          (format-time-string "Today, %H:%M" time))
         ((not (time-less-p time yesterday-start))
          (format-time-string "Yesterday, %H:%M" time))
         ((= timestamp-year current-year)
          (format-time-string "%b %d, %H:%M" time))
         (t
          (format-time-string "%b %d, %Y" time))))
    (error (or timestamp "unknown"))))

(defun ghostel-agent--session-choice-label (session)
  "Return a display label for SESSION."
  (let* ((title (ghostel-agent--truncate-title (ghostel-agent-session-title session)))
         (date (ghostel-agent--format-session-date (ghostel-agent-session-updated session)))
         (id (ghostel-agent-session-id session))
         (title-width 50)
         (date-width 18)
         (title-padded (concat title (make-string (max 0 (- title-width (length title))) ?\s))))
    (format "%s  %s  %s" title-padded date id)))

(defun ghostel-agent--prompt-select-session (sessions &optional agent-name)
  "Prompt to resume one of SESSIONS or start a new session.

Return a `ghostel-agent-session' struct, or nil for a new session."
  (let* ((new-choice ghostel-agent--new-session-label)
         (prompt (if agent-name
                     (format "Start %s (default: new): " agent-name)
                   "Start agent (default: new): "))
         (choices (cons new-choice
                        (mapcar #'ghostel-agent--session-choice-label sessions)))
         (lookup (let ((table (make-hash-table :test 'equal)))
                   (puthash new-choice nil table)
                   (dolist (session sessions)
                     (puthash (ghostel-agent--session-choice-label session) session table))
                   table))
         (this-command 'ghostel-agent--prompt-select-session))
    (let ((selection (completing-read
                      prompt
                      (lambda (string pred action)
                        (if (eq action 'metadata)
                            '(metadata
                              (display-sort-function . identity)
                              (eager-display . t)
                              (eager-update . t))
                          (complete-with-action action choices string pred)))
                      nil t nil nil
                      new-choice)))
      (gethash selection lookup))))

(defun ghostel-agent--sort-sessions (sessions)
  "Return SESSIONS sorted by mtime, newest first."
  (sort (copy-sequence sessions)
        (lambda (a b)
          (> (or (ghostel-agent-session-mtime a) 0)
             (or (ghostel-agent-session-mtime b) 0)))))

(defun ghostel-agent--claude-project-key (dir)
  "Return Claude Code project directory key for DIR."
  (replace-regexp-in-string
   "\\." "-"
   (replace-regexp-in-string "/" "-" (directory-file-name (expand-file-name dir)))))

(defun ghostel-agent--cursor-project-key (dir)
  "Return Cursor project directory key for DIR."
  (let ((encoded (replace-regexp-in-string
                  "/" "-"
                  (replace-regexp-in-string
                   "^/" ""
                   (directory-file-name (expand-file-name dir))))))
    (replace-regexp-in-string "\\." "-" (replace-regexp-in-string "-\\." "-" encoded))))

(defun ghostel-agent--agy-project-slug (dir)
  "Return Antigravity tmp slug for DIR."
  (let ((base (file-name-nondirectory (directory-file-name (expand-file-name dir)))))
    (replace-regexp-in-string
     "\\." "-"
     (if (string-prefix-p "." base)
         (substring base 1)
       base))))

(defun ghostel-agent--json-key (key)
  "Normalize JSON object key to a symbol."
  (if (symbolp key) key (intern key)))

(defun ghostel-agent--json-field (obj key)
  "Return value for KEY from alist or hash-table OBJ."
  (let ((key (ghostel-agent--json-key key)))
    (cond
     ((hash-table-p obj) (gethash key obj))
     ((listp obj) (alist-get key obj)))))

(defun ghostel-agent--json-string-field (obj &rest keys)
  "Return the first non-empty string value for KEYS in OBJ."
  (cl-loop for key in keys
           for value = (ghostel-agent--json-field obj key)
           when (and (stringp value) (not (string-empty-p value)))
           return value))

(defun ghostel-agent--json-array-items (value)
  "Return a list of items from JSON array VALUE."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun ghostel-agent--first-user-text (obj)
  "Extract first user-visible text from JSON object OBJ."
  (let ((type (ghostel-agent--json-field obj "type")))
    (cond
     ((member type '(user human))
      (let* ((message (or (ghostel-agent--json-field obj "message")
                          (ghostel-agent--json-field obj "content")))
             (parts (cond
                      ((stringp message) (list message))
                      ((listp message)
                       (mapcar (lambda (part)
                                 (or (ghostel-agent--json-field part "text")
                                     (and (stringp part) part)))
                               (ghostel-agent--json-array-items message)))
                      (t nil))))
        (cl-loop for text in parts
                 when (and (stringp text) (not (string-empty-p (string-trim text))))
                 return (ghostel-agent--clean-display-text text))))
     ((and (stringp (ghostel-agent--json-field obj "content"))
           (not (string-empty-p (string-trim (ghostel-agent--json-field obj "content")))))
      (ghostel-agent--clean-display-text (ghostel-agent--json-field obj "content")))
     (t (ghostel-agent--json-string-field obj "customTitle" "summary" "lastPrompt")))))

(defun ghostel-agent--parse-claude-session-file (file project-root)
  "Parse Claude session metadata from transcript FILE under PROJECT-ROOT."
  (let ((session-id (file-name-base file))
        (mtime (float-time (file-attribute-modification-time (file-attributes file))))
        (cwd nil)
        (title nil))
    (ghostel-agent--jsonl-each
     file
     (lambda (obj)
       (unless cwd
         (setq cwd (ghostel-agent--json-field obj "cwd")))
       (unless title
         (setq title (ghostel-agent--first-user-text obj)))))
    (when (or (null cwd)
              (ghostel-agent--directory-in-project-p cwd project-root))
      (ghostel-agent-session-create
       :id session-id
       :title (or title session-id)
       :mtime mtime
       :updated mtime))))

(defun ghostel-agent--list-claude-sessions (project-root)
  "List Claude Code sessions for PROJECT-ROOT."
  (let* ((key (ghostel-agent--claude-project-key project-root))
         (dir (expand-file-name key (expand-file-name "~/.claude/projects/")))
         (sessions nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir "\\`.jsonl\\'" nil t))
        (when (file-regular-p file)
          (when-let* ((session (ghostel-agent--parse-claude-session-file file project-root)))
            (push session sessions)))))
    (ghostel-agent--sort-sessions sessions)))

(defun ghostel-agent--parse-cursor-session-file (file)
  "Parse Cursor session metadata from transcript FILE."
  (let ((session-id (file-name-base file))
        (mtime (float-time (file-attribute-modification-time (file-attributes file))))
        (title nil))
    (ghostel-agent--jsonl-each
     file
     (lambda (obj)
       (when (and (not title) (equal (ghostel-agent--json-field obj "role") "user"))
         (let* ((message (ghostel-agent--json-field obj "message"))
                (content (and (listp message) (ghostel-agent--json-field message "content")))
                (parts (ghostel-agent--json-array-items content)))
           (dolist (part parts)
             (when (and (not title)
                        (equal (ghostel-agent--json-field part "type") "text")
                        (stringp (ghostel-agent--json-field part "text")))
               (setq title (ghostel-agent--clean-display-text
                            (ghostel-agent--json-field part "text")))))))))
    (ghostel-agent-session-create
     :id session-id
     :title (or title session-id)
     :mtime mtime
     :updated mtime)))

(defun ghostel-agent--list-cursor-sessions (project-root)
  "List Cursor agent sessions for PROJECT-ROOT."
  (let* ((key (ghostel-agent--cursor-project-key project-root))
         (dir (expand-file-name
               (format "%s/agent-transcripts" key)
               (expand-file-name "~/.cursor/projects/")))
         (sessions nil))
    (when (file-directory-p dir)
      (dolist (session-dir (directory-files dir "^[^.].*" nil t))
        (let ((transcript (expand-file-name
                           (format "%s.jsonl"
                                    (file-name-nondirectory (directory-file-name session-dir)))
                           session-dir)))
          (when (file-readable-p transcript)
            (push (ghostel-agent--parse-cursor-session-file transcript) sessions)))))
    (ghostel-agent--sort-sessions sessions)))

(defun ghostel-agent--fetch-opencode-sessions-json (project-root)
  "Return parsed OpenCode session list JSON for PROJECT-ROOT, or nil."
  (when (executable-find "opencode")
    (let ((default-directory project-root))
      (with-temp-buffer
        (when (and (zerop (call-process "opencode" nil t nil "session" "list" "--format" "json"))
                   (> (point-max) 0))
          (condition-case nil
              (json-parse-string (buffer-string) :object-type 'alist)
            (error nil)))))))

(defun ghostel-agent--list-opencode-sessions (project-root)
  "List OpenCode sessions for PROJECT-ROOT."
  (let ((sessions nil))
    (dolist (item (ghostel-agent--json-array-items
                   (ghostel-agent--fetch-opencode-sessions-json project-root)))
      (let* ((directory (ghostel-agent--json-field item "directory"))
             (id (ghostel-agent--json-field item "id"))
             (title (or (ghostel-agent--json-field item "title") id))
             (updated-ms (ghostel-agent--json-field item "updated"))
             (mtime (if updated-ms (/ (float updated-ms) 1000.0) 0.0)))
        (when (and id
                   (ghostel-agent--directory-in-project-p directory project-root))
          (push (ghostel-agent-session-create
                 :id id
                 :title title
                 :mtime mtime
                 :updated mtime)
                sessions))))
    (ghostel-agent--sort-sessions sessions)))

(defun ghostel-agent--parse-antigravity-session-file (file)
  "Parse Antigravity session metadata from chat FILE."
  (let ((session-id nil)
        (mtime (float-time (file-attribute-modification-time (file-attributes file))))
        (updated nil)
        (title nil))
    (ghostel-agent--jsonl-each
     file
     (lambda (obj)
       (unless session-id
         (setq session-id (ghostel-agent--json-field obj "sessionId")))
       (when (ghostel-agent--json-field obj "lastUpdated")
         (setq updated (ghostel-agent--json-field obj "lastUpdated")))
       (when (and (not title) (equal (ghostel-agent--json-field obj "type") "user"))
         (let ((content (ghostel-agent--json-array-items
                         (ghostel-agent--json-field obj "content"))))
           (dolist (part content)
             (when (and (not title) (stringp (ghostel-agent--json-field part "text")))
               (setq title (ghostel-agent--clean-display-text
                            (ghostel-agent--json-field part "text")))))))))
    (when session-id
      (let ((updated-time (or updated mtime)))
        (ghostel-agent-session-create
         :id session-id
         :title (or title session-id)
         :mtime (if (stringp updated-time)
                    (float-time (date-to-time updated-time))
                  (if (numberp updated-time) updated-time mtime))
         :updated updated-time)))))

(defun ghostel-agent--list-antigravity-sessions (project-root)
  "List Antigravity sessions for PROJECT-ROOT."
  (let* ((slug (ghostel-agent--agy-project-slug project-root))
         (dir (expand-file-name
               (format "~/.gemini/tmp/%s/chats" slug)))
         (sessions nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir "\\`session-.+\\.jsonl\\'" nil t))
        (when (file-regular-p file)
          (when-let* ((session (ghostel-agent--parse-antigravity-session-file file)))
            (push session sessions)))))
    (ghostel-agent--sort-sessions sessions)))

(cl-defun ghostel-agent--run-with-session (&key agent-name list-fn base-command resume-arg buffer-pattern)
  "Start AGENT-NAME in Ghostel, optionally resuming a prior session.

LIST-FN receives the project root and returns a list of
`ghostel-agent-session' structs.  BASE-COMMAND is the shell command
without resume arguments.  RESUME-ARG is the CLI flag used before the
session id (for example \"--resume\")."
  (let* ((project-root (ghostel-agent--project-root))
         (buffer-name (replace-regexp-in-string
                        "%p"
                        (lx/run-in-projectile-ghostel--scope-label)
                        buffer-pattern))
         (existing (lx/run-in-projectile-ghostel--find-buffer buffer-name project-root)))
    (if existing
        (if (equal existing (current-buffer))
            (if (and (= 1 (length (window-list)))
                     (eq (selected-window) (car (window-list))))
                (bury-buffer)
              (delete-window))
          (pop-to-buffer existing 'display-buffer-pop-up-window))
      (let* ((sessions (funcall list-fn project-root))
             (session (ghostel-agent--prompt-select-session sessions agent-name))
             (session-id (and session (ghostel-agent-session-id session)))
             (command (if session-id
                          (format "%s %s %s" base-command resume-arg
                                  (shell-quote-argument session-id))
                        base-command)))
        (lx/run-in-projectile-ghostel command buffer-name project-root)))))

(defun ghostel-agent-run-claude ()
  "Start a Claude agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "Claude"
   :list-fn #'ghostel-agent--list-claude-sessions
   :base-command (format "env DISABLE_INSTALLATION_CHECKS=1 ANTHROPIC_BASE_URL=https://open.bigmodel.cn/api/anthropic ANTHROPIC_MODEL=glm-5.1 ANTHROPIC_DEFAULT_OPUS_MODEL=glm-5.1 ANTHROPIC_DEFAULT_SONNET_MODEL=glm-5-turbo ANTHROPIC_DEFAULT_HAIKU_MODEL=glm-4.7 ANTHROPIC_AUTH_TOKEN=%s claude --dangerously-skip-permissions" (getenv "ZHIPU_API_KEY"))
   :resume-arg "--resume"
   :buffer-pattern "*ghostel-claude[%p]*"))

(defun ghostel-agent-run-aliyun-token-plan-claude ()
  "Start a Aliyun Token Plan Claude agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "Aliyun Token Plan"
   :list-fn #'ghostel-agent--list-claude-sessions
   :base-command (format "env DISABLE_INSTALLATION_CHECKS=1 ANTHROPIC_BASE_URL=https://token-plan.cn-beijing.maas.aliyuncs.com/apps/anthropic ANTHROPIC_MODEL=qwen3.7-max ANTHROPIC_DEFAULT_OPUS_MODEL=qwen3.7-max ANTHROPIC_DEFAULT_SONNET_MODEL=qwen3.6-plus ANTHROPIC_DEFAULT_HAIKU_MODEL=qwen3.6-flash ANTHROPIC_AUTH_TOKEN=%s claude --dangerously-skip-permissions" (getenv "ALIYUN_TOKEN_PLAN_API_KEY"))
   :resume-arg "--resume"
   :buffer-pattern "*ghostel-aliyun-tp-claude[%p]*"))

(defun ghostel-agent-run-deepseek-claude ()
  "Start a Aliyun Token Plan Claude agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "DeepSeek"
   :list-fn #'ghostel-agent--list-claude-sessions
   :base-command (format "env DISABLE_INSTALLATION_CHECKS=1 ANTHROPIC_BASE_URL=https://api.deepseek.com/anthropic ANTHROPIC_MODEL=deepseek-v4-pro[1m] ANTHROPIC_DEFAULT_OPUS_MODEL=deepseek-v4-pro[1m] ANTHROPIC_DEFAULT_SONNET_MODEL=deepseek-v4-flash[1m] ANTHROPIC_DEFAULT_HAIKU_MODEL=deepseek-v4-flash[1m] ANTHROPIC_AUTH_TOKEN=%s claude --dangerously-skip-permissions" (getenv "DEEPSEEK_API_KEY"))
   :resume-arg "--resume"
   :buffer-pattern "*ghostel-deepseek-claude[%p]*"))

(defun ghostel-agent-run-opencode ()
  "Start an Opencode agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "OpenCode"
   :list-fn #'ghostel-agent--list-opencode-sessions
   :base-command "opencode"
   :resume-arg "-s"
   :buffer-pattern "*ghostel-opencode[%p]*"))

(defun ghostel-agent-run-cursor ()
  "Start a Cursor agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "Cursor"
   :list-fn #'ghostel-agent--list-cursor-sessions
   :base-command "cursor-agent --force"
   :resume-arg "--resume"
   :buffer-pattern "*ghostel-cursor[%p]*"))

(defun ghostel-agent-run-antigravity ()
  "Start an Antigravity agent shell in Ghostel."
  (interactive)
  (ghostel-agent--run-with-session
   :agent-name "Antigravity"
   :list-fn #'ghostel-agent--list-antigravity-sessions
   :base-command "agy --dangerously-skip-permissions"
   :resume-arg "--conversation"
   :buffer-pattern "*ghostel-agy[%p]*"))

(defalias 'ghostel-agent-run-agy #'ghostel-agent-run-antigravity
  "Compatibility alias bound in `global-set-key.el'.")

(defun ghostel-agent-send ()
  "Send the current file or selection to a visible Ghostel agent shell.
If the region is not active, sends @ABSOLUTE_FILE_PATH.
Otherwise sends \"ABSOLUTE_FILE_PATH 中的第 M-N 行:\" plus a newline and the region text.
After sending, deactivate the region when applicable and select the agent buffer window."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let* ((had-region (use-region-p))
         (agent-buffer (ghostel-agent--first-visible-agent-buffer)))
    (unless agent-buffer
      (user-error "No visible Ghostel agent buffer; show claude/opencode/cursor/agy Ghostel first"))
    (let* ((abs-path (expand-file-name (buffer-file-name)))
           (str
            (if had-region
                (let* ((beg (region-beginning))
                       (end (region-end))
                       (lo (min beg end))
                       (hi (max beg end))
                       (hi-line-pos (max lo (1- hi)))
                       (m (save-excursion (goto-char lo) (line-number-at-pos)))
                       (n (save-excursion (goto-char hi-line-pos) (line-number-at-pos)))
                       (selection (buffer-substring-no-properties lo hi)))
                  (concat abs-path " 中的第 "
                          (number-to-string m) "-" (number-to-string n)
                          " 行:\n\n" selection "\n"))
              (concat "@" abs-path "\n"))))
      (with-current-buffer agent-buffer
        (ghostel-send-string str))
      (when had-region
        (deactivate-mark))
      (if-let* ((win (get-buffer-window agent-buffer 'visible)))
          (select-window win)
        (pop-to-buffer agent-buffer)))))

(provide 'ghostel-agent)
