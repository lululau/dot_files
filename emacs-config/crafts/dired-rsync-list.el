;;; dired-rsync-list.el --- Tablist-based process monitor for dired-rsync -*- lexical-binding: t; -*-

;; Copyright (C) 2026 liuxiang

;; Author: liuxiang
;; Keywords: dired, rsync, processes, tablist
;; Package-Requires: ((emacs "27.1") (tablist "0.70") (dired-rsync "0.6"))

;;; Commentary:

;; This module provides a `tablist'-based monitor and management buffer
;; for rsync processes started by `dired-rsync'.
;;
;; Features:
;; - Intercepts `dired-rsync' processes and keeps track of metadata (PID, source,
;;   destination, command, start/end time, duration, status).
;; - Real-time parsing of `rsync --info=progress2' metrics (bytes, percent,
;;   speed, ETA, and file progress).
;; - Retains full output logs even after `dired-rsync' finishes and kills its
;;   process buffer.
;; - Row actions: stop/kill process, view output log, retry job, jump to source
;;   or destination in Dired, clear completed history.
;; - Smart auto-refresh using an on-demand timer that only runs when the list
;;   is visible and active jobs are running (zero background CPU usage).

;;; Code:

(require 'cl-lib)
(require 'tablist)
(require 'dired)
(require 'dired-rsync)
(require 's)

;;; Customization & Variables

(defgroup dired-rsync-list nil
  "Tablist monitor for dired-rsync processes."
  :group 'dired-rsync
  :prefix "dired-rsync-list-")

(defcustom dired-rsync-list-max-jobs 50
  "Maximum number of jobs to retain in history."
  :type 'integer
  :group 'dired-rsync-list)

(defcustom dired-rsync-list-refresh-interval 1.0
  "Refresh interval in seconds for the *dired-rsync-list* buffer."
  :type 'float
  :group 'dired-rsync-list)

(defcustom dired-rsync-list-source-max-length 30
  "Maximum length for source column string before truncation."
  :type 'integer
  :group 'dired-rsync-list)

(defcustom dired-rsync-list-dest-max-length 30
  "Maximum length for destination column string before truncation."
  :type 'integer
  :group 'dired-rsync-list)

;;; Data Model

(cl-defstruct (dired-rsync-job (:constructor dired-rsync-job--create))
  id                ;; Integer ID (1, 2, 3...)
  pid               ;; System PID (integer or nil)
  process           ;; Emacs process object (or nil when dead)
  status            ;; 'running | 'finished | 'failed | 'killed
  source            ;; Short display string
  source-files      ;; List of original source file paths
  dest              ;; Destination directory/path
  cmd               ;; Full rsync shell command string
  start-time        ;; Float timestamp (float-time)
  end-time          ;; Float timestamp or nil
  exit-code         ;; Exit code or signal description
  bytes-transferred ;; e.g. "1.25G"
  percent           ;; e.g. "45%"
  speed             ;; e.g. "32.50MB/s"
  eta               ;; e.g. "0:01:23"
  file-progress     ;; e.g. "(xfr#5, to-chk=12/34)"
  log-output        ;; Cached full output log string
  dired-buffer      ;; Source Dired buffer (if still alive)
  proc-buffer-name  ;; Original process buffer name
  )

(defvar dired-rsync-list--jobs nil
  "List of `dired-rsync-job' structs, newest first.")

(defvar dired-rsync-list--next-id 1
  "Auto-incrementing counter for job IDs.")

(defvar dired-rsync-list--timer nil
  "Refresh timer for *dired-rsync-list*.")

(defvar dired-rsync-list--current-context nil
  "Dynamically scoped context plist during `dired-rsync' invocation.")

;;; Regular Expressions

(defconst dired-rsync-list--progress2-regex
  (rx (group (+ (any digit ",." "KMGTPEZYkmgtpezy"))) ;; 1: bytes
      (+ (any " \t"))
      (group (+ digit) "%")                          ;; 2: percent
      (+ (any " \t"))
      (group (+ (any digit ".KMGTPEZYkmgtpezyB/s" "b/s"))) ;; 3: speed
      (+ (any " \t"))
      (group (+ digit) ":" (+ digit) (? ":" (+ digit)))   ;; 4: eta
      (? (+ (any " \t"))
         (group "(" (+ (not (any ")\n\r"))) ")")))        ;; 5: files-info
  "Regex to extract rsync --info=progress2 output fields.")

;;; Helper Functions

(defun dired-rsync-list--find-job-by-id (id)
  "Find job with ID in `dired-rsync-list--jobs'."
  (cl-find id dired-rsync-list--jobs :key #'dired-rsync-job-id))

(defun dired-rsync-list--find-job-by-process (proc)
  "Find job associated with Emacs process PROC."
  (or (and (processp proc) (process-get proc 'dired-rsync-job))
      (cl-find proc dired-rsync-list--jobs :key #'dired-rsync-job-process)))

(defun dired-rsync-list--active-jobs-p ()
  "Return non-nil if any job in `dired-rsync-list--jobs' is 'running."
  (cl-some (lambda (j) (eq (dired-rsync-job-status j) 'running))
           dired-rsync-list--jobs))

(defun dired-rsync-list--format-time (time)
  "Format timestamp TIME into \"MM-DD HH:MM:SS\"."
  (if (or (null time) (and (numberp time) (zerop time)))
      "--:--:--"
    (format-time-string "%m-%d %H:%M:%S"
                        (if (numberp time) (seconds-to-time time) time))))

(defun dired-rsync-list--format-duration (start end)
  "Format duration between START and END into HH:MM:SS or MM:SS."
  (if (null start)
      "--:--"
    (let* ((end-time (or end (float-time)))
           (secs (max 0 (floor (- end-time start))))
           (hours (/ secs 3600))
           (mins (/ (% secs 3600) 60))
           (seconds (% secs 60)))
      (if (> hours 0)
          (format "%02d:%02d:%02d" hours mins seconds)
        (format "%02d:%02d" mins seconds)))))

(defun dired-rsync-list--format-source-files (files)
  "Format source FILES into a concise display string."
  (cond
   ((null files) "N/A")
   ((= (length files) 1)
    (file-name-nondirectory (directory-file-name (car files))))
   (t
    (format "%s (+%d)"
            (file-name-nondirectory (directory-file-name (car files)))
            (1- (length files))))))

(defun dired-rsync-list--truncate-string (str max-len)
  "Truncate STR to MAX-LEN characters with ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 3)) "...")
    (or str "")))

;;; Progress2 Parser

(defun dired-rsync-list--parse-progress2 (string)
  "Parse rsync progress2 output from STRING.
Return a plist (:bytes :percent :speed :eta :file-progress) or nil."
  (when (and string (string-match dired-rsync-list--progress2-regex string))
    (list :bytes (match-string 1 string)
          :percent (match-string 2 string)
          :speed (match-string 3 string)
          :eta (match-string 4 string)
          :file-progress (match-string 5 string))))

(defun dired-rsync-list--update-job-progress (job string)
  "Update JOB metrics from incoming rsync output STRING."
  (when-let* ((parsed (dired-rsync-list--parse-progress2 string)))
    (when (plist-get parsed :bytes)
      (setf (dired-rsync-job-bytes-transferred job) (plist-get parsed :bytes)))
    (when (plist-get parsed :percent)
      (setf (dired-rsync-job-percent job) (plist-get parsed :percent)))
    (when (plist-get parsed :speed)
      (setf (dired-rsync-job-speed job) (plist-get parsed :speed)))
    (when (plist-get parsed :eta)
      (setf (dired-rsync-job-eta job) (plist-get parsed :eta)))
    (when (plist-get parsed :file-progress)
      (setf (dired-rsync-job-file-progress job) (plist-get parsed :file-progress)))))

;;; Job Management

(defun dired-rsync-list--add-job (job)
  "Add JOB to `dired-rsync-list--jobs' and enforce max limit."
  (push job dired-rsync-list--jobs)
  ;; Enforce max jobs limit
  (when (> (length dired-rsync-list--jobs) dired-rsync-list-max-jobs)
    ;; Remove oldest non-running jobs first
    (let* ((to-drop (- (length dired-rsync-list--jobs) dired-rsync-list-max-jobs))
           (reversed (reverse dired-rsync-list--jobs))
           (filtered nil))
      (dolist (j reversed)
        (if (and (> to-drop 0) (not (eq (dired-rsync-job-status j) 'running)))
            (setq to-drop (1- to-drop))
          (push j filtered)))
      (setq dired-rsync-list--jobs filtered))))

(defun dired-rsync-list--create-job-from-context (command details)
  "Instantiate a `dired-rsync-job' from COMMAND and DETAILS."
  (let* ((sfiles (or (plist-get details :marked-files)
                     (plist-get dired-rsync-list--current-context :sfiles)))
         (dest (or (plist-get dired-rsync-list--current-context :dest)
                   "N/A"))
         (dired-buf (or (plist-get details :dired-buffer)
                        (plist-get dired-rsync-list--current-context :dired-buffer)))
         (id dired-rsync-list--next-id))
    (setq dired-rsync-list--next-id (1+ dired-rsync-list--next-id))
    (dired-rsync-job--create
     :id id
     :status 'running
     :source (dired-rsync-list--format-source-files sfiles)
     :source-files sfiles
     :dest dest
     :cmd command
     :start-time (float-time)
     :dired-buffer dired-buf
     :percent "0%"
     :bytes-transferred "0B"
     :speed "--"
     :eta "--:--")))

;;; Advice Functions for dired-rsync

(defun dired-rsync-list--around-dired-rsync (orig-fun &rest args)
  "Advice around `dired-rsync' to capture source files and destination."
  (let* ((dest (car args))
         (sfiles (funcall dired-rsync-source-files))
         (dired-rsync-list--current-context
          (list :sfiles sfiles
                :dest dest
                :dired-buffer (current-buffer))))
    (apply orig-fun args)))

(defun dired-rsync-list--around-do-run (orig-fun command details)
  "Advice around `dired-rsync--do-run' to capture the process and track the job."
  (let ((job (dired-rsync-list--create-job-from-context command details))
        (proc nil))
    (cl-letf* (((symbol-function 'make-process)
                (let ((orig-make-process (symbol-function 'make-process)))
                  (lambda (&rest plist)
                    (let ((p (apply orig-make-process plist)))
                      (setq proc p)
                      p)))))
      (prog1 (funcall orig-fun command details)
        (when (and job (processp proc))
          (setf (dired-rsync-job-process job) proc)
          (setf (dired-rsync-job-pid job) (process-id proc))
          (setf (dired-rsync-job-proc-buffer-name job) (buffer-name (process-buffer proc)))
          (process-put proc 'dired-rsync-job job)
          (dired-rsync-list--add-job job)
          (dired-rsync-list--maybe-start-timer)
          (dired-rsync-list--trigger-update))))))

(defun dired-rsync-list--filter-advice (proc string)
  "Advice after `dired-rsync--filter' to update job progress."
  (when-let* ((job (dired-rsync-list--find-job-by-process proc)))
    (dired-rsync-list--update-job-progress job string)))

(defun dired-rsync-list--sentinel-advice (proc desc &rest _)
  "Advice before `dired-rsync--sentinel' to capture logs and finalize job."
  (when-let* ((job (dired-rsync-list--find-job-by-process proc)))
    (let ((proc-buf (process-buffer proc)))
      (when (and proc-buf (buffer-live-p proc-buf))
        ;; Save full buffer string before it gets killed
        (with-current-buffer proc-buf
          (setf (dired-rsync-job-log-output job) (buffer-string)))))
    (setf (dired-rsync-job-end-time job) (float-time))
    (setf (dired-rsync-job-exit-code job) (process-exit-status proc))
    (cond
     ((s-starts-with-p "finished" desc)
      (setf (dired-rsync-job-status job) 'finished)
      (setf (dired-rsync-job-percent job) "100%")
      (setf (dired-rsync-job-eta job) "0:00:00"))
     ((or (s-starts-with-p "killed" desc)
          (s-starts-with-p "interrupt" desc)
          (eq (dired-rsync-job-status job) 'killed))
      (setf (dired-rsync-job-status job) 'killed))
     (t
      (setf (dired-rsync-job-status job) 'failed)))
    (dired-rsync-list--trigger-update)))

;;; Sorting Predicates for tabulated-list

(defun dired-rsync-list--sort-id (entry-a entry-b)
  "Sort entries by ID."
  (let ((id-a (car entry-a))
        (id-b (car entry-b)))
    (> (or id-a 0) (or id-b 0))))

(defun dired-rsync-list--sort-pid (entry-a entry-b)
  "Sort entries by PID."
  (let* ((job-a (dired-rsync-list--find-job-by-id (car entry-a)))
         (job-b (dired-rsync-list--find-job-by-id (car entry-b)))
         (pid-a (or (and job-a (dired-rsync-job-pid job-a)) 0))
         (pid-b (or (and job-b (dired-rsync-job-pid job-b)) 0)))
    (> pid-a pid-b)))

(defun dired-rsync-list--sort-progress (entry-a entry-b)
  "Sort entries by percentage progress."
  (let* ((job-a (dired-rsync-list--find-job-by-id (car entry-a)))
         (job-b (dired-rsync-list--find-job-by-id (car entry-b)))
         (pct-a (string-to-number (or (and job-a (dired-rsync-job-percent job-a)) "0")))
         (pct-b (string-to-number (or (and job-b (dired-rsync-job-percent job-b)) "0"))))
    (> pct-a pct-b)))

;;; Tablist Entries & Faces

(defface dired-rsync-list-status-running
  '((t :foreground "#4fc3f7" :weight bold))
  "Face for running rsync jobs."
  :group 'dired-rsync-list)

(defface dired-rsync-list-status-finished
  '((t :foreground "#81c784" :weight bold))
  "Face for finished rsync jobs."
  :group 'dired-rsync-list)

(defface dired-rsync-list-status-failed
  '((t :foreground "#e57373" :weight bold))
  "Face for failed rsync jobs."
  :group 'dired-rsync-list)

(defface dired-rsync-list-status-killed
  '((t :foreground "#ffb74d" :weight bold))
  "Face for killed rsync jobs."
  :group 'dired-rsync-list)

(defun dired-rsync-list--status-face (status)
  "Return face corresponding to STATUS."
  (pcase status
    ('running 'dired-rsync-list-status-running)
    ('finished 'dired-rsync-list-status-finished)
    ('failed 'dired-rsync-list-status-failed)
    ('killed 'dired-rsync-list-status-killed)
    (_ 'default)))

(defun dired-rsync-list--format-status (status)
  "Return propertized status string for STATUS."
  (let ((str (symbol-name status))
        (face (dired-rsync-list--status-face status)))
    (propertize str 'font-lock-face face)))

(defun dired-rsync-list--entries ()
  "Generate `tabulated-list-entries' from `dired-rsync-list--jobs'."
  (mapcar
   (lambda (job)
     (let* ((id (dired-rsync-job-id job))
            (pid (if-let* ((p (dired-rsync-job-pid job)))
                     (number-to-string p)
                   "-"))
            (status (dired-rsync-list--format-status (dired-rsync-job-status job)))
            (pct (or (dired-rsync-job-percent job) "0%"))
            (speed (or (dired-rsync-job-speed job) "--"))
            (transferred (or (dired-rsync-job-bytes-transferred job) "0B"))
            (eta (if (eq (dired-rsync-job-status job) 'running)
                     (or (dired-rsync-job-eta job) "--:--")
                   "--:--"))
            (elapsed (dired-rsync-list--format-duration
                      (dired-rsync-job-start-time job)
                      (dired-rsync-job-end-time job)))
            (source (dired-rsync-list--truncate-string
                     (dired-rsync-job-source job)
                     dired-rsync-list-source-max-length))
            (dest (dired-rsync-list--truncate-string
                   (dired-rsync-job-dest job)
                   dired-rsync-list-dest-max-length))
            (start (dired-rsync-list--format-time (dired-rsync-job-start-time job)))
            (end (dired-rsync-list--format-time (dired-rsync-job-end-time job))))
       (list id
             (vector (number-to-string id)
                     pid
                     status
                     pct
                     speed
                     transferred
                     eta
                     elapsed
                     source
                     dest
                     start
                     end))))
   dired-rsync-list--jobs))

;;; Major Mode Definition

(defvar-keymap dired-rsync-list-mode-map
  :parent tablist-mode-map
  "RET" #'dired-rsync-list-view-output
  "o"   #'dired-rsync-list-view-output
  "k"   #'dired-rsync-list-kill
  "K"   #'dired-rsync-list-force-kill
  "x"   #'dired-rsync-list-kill
  "r"   #'dired-rsync-list-retry
  "s"   #'dired-rsync-list-jump-source
  "d"   #'dired-rsync-list-jump-dest
  "c"   #'dired-rsync-list-clear-finished
  "g"   #'dired-rsync-list-refresh
  "q"   #'quit-window)

(define-derived-mode dired-rsync-list-mode tablist-mode "Rsync-Jobs"
  "Major mode for listing and managing dired-rsync jobs."
  (setq tabulated-list-format
        [("ID" 4 dired-rsync-list--sort-id :right-align t)
         ("PID" 7 dired-rsync-list--sort-pid :right-align t)
         ("Status" 10 t)
         ("Progress" 9 dired-rsync-list--sort-progress :right-align t)
         ("Speed" 12 nil :right-align t)
         ("Transferred" 11 nil :right-align t)
         ("ETA" 9 nil :right-align t)
         ("Elapsed" 9 nil :right-align t)
         ("Source" 26 nil)
         ("Destination" 26 nil)
         ("Start Time" 16 nil)
         ("End Time" 16 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" t))
  (add-hook 'kill-buffer-hook #'dired-rsync-list--on-buffer-killed nil t)
  (tabulated-list-init-header))

;; Evil integration
(with-eval-after-load 'evil
  (evil-define-key 'normal dired-rsync-list-mode-map
    (kbd "RET") #'dired-rsync-list-view-output
    "o"  #'dired-rsync-list-view-output
    "k"  #'evil-previous-line
    "j"  #'evil-next-line
    "K"  #'dired-rsync-list-kill
    "x"  #'dired-rsync-list-kill
    "r"  #'dired-rsync-list-retry
    "s"  #'dired-rsync-list-jump-source
    "d"  #'dired-rsync-list-jump-dest
    "c"  #'dired-rsync-list-clear-finished
    "gr" #'dired-rsync-list-refresh
    "q"  #'quit-window))

;;; Interactive Commands

(defun dired-rsync-list--get-target-jobs ()
  "Return list of marked `dired-rsync-job' instances, or job at point."
  (if-let* ((marked-ids (tablist-get-marked-items)))
      (delq nil (mapcar #'dired-rsync-list--find-job-by-id marked-ids))
    (when-let* ((id (tabulated-list-get-id)))
      (if-let* ((job (dired-rsync-list--find-job-by-id id)))
          (list job)
        nil))))

(defun dired-rsync-list-view-output ()
  "View process output for the job at point.
If the job is running and its buffer is alive, switch to it.
Otherwise, display cached output log in a read-only buffer."
  (interactive)
  (let ((job (or (and (tabulated-list-get-id)
                      (dired-rsync-list--find-job-by-id (tabulated-list-get-id)))
                 (user-error "No job at point"))))
    (let ((proc (dired-rsync-job-process job)))
      (if (and proc (process-live-p proc) (buffer-live-p (process-buffer proc)))
          ;; Running job: pop to live buffer
          (pop-to-buffer (process-buffer proc))
        ;; Finished or buffer killed: pop up read-only view
        (let* ((buf-name (format "*dired-rsync-log: #%s*" (dired-rsync-job-id job)))
               (log-buf (get-buffer-create buf-name))
               (log-content (or (dired-rsync-job-log-output job)
                                "No log output recorded for this job.")))
          (with-current-buffer log-buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "=== Dired Rsync Job #%s ===\n" (dired-rsync-job-id job)))
              (insert (format "PID:         %s\n" (or (dired-rsync-job-pid job) "N/A")))
              (insert (format "Status:      %s (Exit code: %s)\n"
                              (dired-rsync-job-status job)
                              (or (dired-rsync-job-exit-code job) "N/A")))
              (insert (format "Command:     %s\n" (or (dired-rsync-job-cmd job) "N/A")))
              (insert (format "Source:      %s\n" (or (dired-rsync-job-source job) "N/A")))
              (insert (format "Destination: %s\n" (or (dired-rsync-job-dest job) "N/A")))
              (insert (format "Start time:  %s\n"
                              (dired-rsync-list--format-time (dired-rsync-job-start-time job))))
              (insert (format "End time:    %s\n"
                              (dired-rsync-list--format-time (dired-rsync-job-end-time job))))
              (insert (format "Elapsed:     %s\n"
                              (dired-rsync-list--format-duration
                               (dired-rsync-job-start-time job)
                               (dired-rsync-job-end-time job))))
              (insert "\n=== Output Log ===\n\n")
              (insert log-content))
            (special-mode)
            (goto-char (point-min)))
          (pop-to-buffer log-buf))))))

(defun dired-rsync-list-kill (&optional sigkill)
  "Kill rsync processes for marked jobs or the job at point.
If SIGKILL is non-nil (or with prefix arg), sends SIGKILL (9),
otherwise sends SIGTERM (15)."
  (interactive "P")
  (let* ((jobs (dired-rsync-list--get-target-jobs))
         (running-jobs (cl-remove-if-not (lambda (j) (eq (dired-rsync-job-status j) 'running)) jobs)))
    (if (null running-jobs)
        (message "No running rsync processes selected")
      (let* ((pids (delq nil (mapcar #'dired-rsync-job-pid running-jobs)))
             (signal (if sigkill 9 15))
             (sig-name (if sigkill "SIGKILL" "SIGTERM"))
             (prompt (format "Send %s to rsync PID(s): %s? "
                             sig-name
                             (mapconcat #'number-to-string pids ", "))))
        (when (y-or-n-p prompt)
          (dolist (job running-jobs)
            (when-let* ((proc (dired-rsync-job-process job)))
              (when (process-live-p proc)
                (interrupt-process proc)))
            (when-let* ((pid (dired-rsync-job-pid job)))
              (ignore-errors (signal-process pid signal)))
            (setf (dired-rsync-job-status job) 'killed)
            (setf (dired-rsync-job-end-time job) (float-time)))
          (dired-rsync-list-refresh)
          (message "Killed %d process(es)" (length running-jobs)))))))

(defun dired-rsync-list-force-kill ()
  "Force kill (SIGKILL) rsync processes for marked jobs or job at point."
  (interactive)
  (dired-rsync-list-kill t))

(defun dired-rsync-list-retry ()
  "Retry the job at point with identical parameters."
  (interactive)
  (let ((job (or (and (tabulated-list-get-id)
                      (dired-rsync-list--find-job-by-id (tabulated-list-get-id)))
                 (user-error "No job at point"))))
    (if (eq (dired-rsync-job-status job) 'running)
        (user-error "Job #%s is currently running" (dired-rsync-job-id job))
      (let ((cmd (dired-rsync-job-cmd job))
            (sfiles (dired-rsync-job-source-files job))
            (dest (dired-rsync-job-dest job))
            (dired-buf (dired-rsync-job-dired-buffer job)))
        (when (y-or-n-p (format "Retry rsync job #%s to %s? " (dired-rsync-job-id job) dest))
          (let ((dired-rsync-list--current-context
                 (list :sfiles sfiles :dest dest :dired-buffer dired-buf)))
            (dired-rsync--do-run cmd
                                 (list :marked-files sfiles
                                       :dired-buffer dired-buf))
            (message "Job #%s restarted" (dired-rsync-job-id job))))))))

(defun dired-rsync-list-jump-source ()
  "Jump to the source file or directory in Dired."
  (interactive)
  (let* ((job (or (and (tabulated-list-get-id)
                       (dired-rsync-list--find-job-by-id (tabulated-list-get-id)))
                  (user-error "No job at point")))
         (sfiles (dired-rsync-job-source-files job))
         (first-file (car sfiles)))
    (cond
     ((and (dired-rsync-job-dired-buffer job)
           (buffer-live-p (dired-rsync-job-dired-buffer job)))
      (pop-to-buffer (dired-rsync-job-dired-buffer job)))
     (first-file
      (dired (file-name-directory (directory-file-name first-file)))
      (dired-goto-file first-file))
     (t
      (message "No source file information available")))))

(defun dired-rsync-list-jump-dest ()
  "Jump to the destination directory in Dired."
  (interactive)
  (let* ((job (or (and (tabulated-list-get-id)
                       (dired-rsync-list--find-job-by-id (tabulated-list-get-id)))
                  (user-error "No job at point")))
         (dest (dired-rsync-job-dest job)))
    (if (and dest (not (string= dest "N/A")))
        (dired (file-name-as-directory dest))
      (message "No destination directory available"))))

(defun dired-rsync-list-clear-finished ()
  "Clear all finished, killed, or failed jobs from the list."
  (interactive)
  (let ((count 0))
    (setq dired-rsync-list--jobs
          (cl-remove-if (lambda (j)
                          (unless (eq (dired-rsync-job-status j) 'running)
                            (cl-incf count)
                            t))
                        dired-rsync-list--jobs))
    (dired-rsync-list-refresh)
    (message "Cleared %d finished job(s)" count)))

;;; Refresh & Timer Mechanism

(defun dired-rsync-list--refresh-buffer ()
  "Refresh the *dired-rsync-list* buffer preserving marks and point."
  (when (derived-mode-p 'tablist-mode)
    (setq tabulated-list-entries (dired-rsync-list--entries))
    (tablist-save-marks
      (tablist-with-remembering-entry
        (tabulated-list-print t)))))

(defun dired-rsync-list--timer-callback ()
  "Timer callback to refresh *dired-rsync-list* if visible and active."
  (let ((buf (get-buffer "*dired-rsync-list*")))
    (if (and buf (buffer-live-p buf) (get-buffer-window buf t))
        (if (dired-rsync-list--active-jobs-p)
            (with-current-buffer buf
              (dired-rsync-list--refresh-buffer))
          ;; No running jobs, stop timer
          (dired-rsync-list--stop-timer))
      ;; Buffer hidden or killed, stop timer
      (dired-rsync-list--stop-timer))))

(defun dired-rsync-list--maybe-start-timer ()
  "Start refresh timer if needed."
  (when (and (null dired-rsync-list--timer)
             (dired-rsync-list--active-jobs-p)
             (get-buffer-window "*dired-rsync-list*" t))
    (setq dired-rsync-list--timer
          (run-at-time dired-rsync-list-refresh-interval
                       dired-rsync-list-refresh-interval
                       #'dired-rsync-list--timer-callback))))

(defun dired-rsync-list--stop-timer ()
  "Stop refresh timer."
  (when dired-rsync-list--timer
    (cancel-timer dired-rsync-list--timer)
    (setq dired-rsync-list--timer nil)))

(defun dired-rsync-list--on-buffer-killed ()
  "Hook called when *dired-rsync-list* buffer is killed."
  (dired-rsync-list--stop-timer))

(defun dired-rsync-list--trigger-update ()
  "Trigger an immediate UI update if list buffer is live."
  (when-let* ((buf (get-buffer "*dired-rsync-list*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (dired-rsync-list--refresh-buffer))))
  (dired-rsync-list--maybe-start-timer))

(defun dired-rsync-list-refresh ()
  "Manually refresh the *dired-rsync-list* buffer."
  (interactive)
  (let ((buf (get-buffer "*dired-rsync-list*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (dired-rsync-list--refresh-buffer))))
  (dired-rsync-list--maybe-start-timer))

;;; Entry Point Command

;;;###autoload
(defun dired-rsync-list ()
  "Display the list of dired-rsync processes."
  (interactive)
  (let ((buf (get-buffer-create "*dired-rsync-list*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'dired-rsync-list-mode)
        (dired-rsync-list-mode))
      (dired-rsync-list--refresh-buffer))
    (pop-to-buffer buf)
    (dired-rsync-list--maybe-start-timer)))

;;; Lifecycle Advice Setup & Teardown

;;;###autoload
(defun dired-rsync-list-setup ()
  "Enable advice hooks to monitor dired-rsync jobs."
  (interactive)
  (advice-add 'dired-rsync :around #'dired-rsync-list--around-dired-rsync)
  (advice-add 'dired-rsync--do-run :around #'dired-rsync-list--around-do-run)
  (advice-add 'dired-rsync--filter :after #'dired-rsync-list--filter-advice)
  (advice-add 'dired-rsync--sentinel :before #'dired-rsync-list--sentinel-advice))

;;;###autoload
(defun dired-rsync-list-teardown ()
  "Disable advice hooks and cancel timers."
  (interactive)
  (advice-remove 'dired-rsync #'dired-rsync-list--around-dired-rsync)
  (advice-remove 'dired-rsync--do-run #'dired-rsync-list--around-do-run)
  (advice-remove 'dired-rsync--filter #'dired-rsync-list--filter-advice)
  (advice-remove 'dired-rsync--sentinel #'dired-rsync-list--sentinel-advice)
  (dired-rsync-list--stop-timer))

;; Automatically activate advice hooks upon loading
(dired-rsync-list-setup)

(provide 'dired-rsync-list)
;;; dired-rsync-list.el ends here
