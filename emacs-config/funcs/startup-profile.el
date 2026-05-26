;;; startup-profile.el --- Spacemacs startup timing instrumentation  -*- lexical-binding: t; -*-

(defvar lx/startup-profile--log-file
  (expand-file-name "startup-profile.log"
                    (or (and (boundp 'spacemacs-cache-directory)
                             spacemacs-cache-directory)
                        "~/.emacs.spacemacs.d/"))
  "File where startup timing events are appended.")

(defvar lx/startup-profile--events nil
  "Collected startup events as (LABEL . SECS-SINCE-ORIGIN) pairs.")

(defvar lx/startup-profile--origin nil
  "Wall-clock time when profiling started.")

(defun lx/startup-profile--now ()
  (float-time))

(defun lx/startup-profile--elapsed ()
  (- (lx/startup-profile--now) lx/startup-profile--origin))

(defun lx/startup-profile--ensure-origin ()
  (unless lx/startup-profile--origin
    (setq lx/startup-profile--origin (lx/startup-profile--now))))

(defun lx/startup-profile-mark (label)
  "Record LABEL with elapsed seconds since profiling origin."
  (lx/startup-profile--ensure-origin)
  (push (cons label (lx/startup-profile--elapsed))
        lx/startup-profile--events))

(defun lx/startup-profile-time (label thunk)
  "Evaluate THUNK, record LABEL duration, return its value."
  (lx/startup-profile--ensure-origin)
  (let* ((start (lx/startup-profile--now))
         (result (funcall thunk))
         (duration (- (lx/startup-profile--now) start)))
    (push (cons label duration) lx/startup-profile--events)
    result))

(defun lx/startup-profile-load-files (label directory)
  "Load *.el in DIRECTORY except init.el, recording per-file timings under LABEL."
  (lx/startup-profile--ensure-origin)
  (let ((dir (file-name-as-directory directory))
        (total-start (lx/startup-profile--now))
        file-start duration)
    (dolist (file (directory-files dir t "\\.el$"))
      (unless (string-match-p "init\\.el$" file)
        (setq file-start (lx/startup-profile--now))
        (load-file file)
        (setq duration (- (lx/startup-profile--now) file-start))
        (when (> duration 0.01)
          (push (cons (format "%s/%s" label (file-name-nondirectory file)) duration)
                lx/startup-profile--events))))
    (push (cons label (- (lx/startup-profile--now) total-start))
          lx/startup-profile--events)))

(defun lx/startup-profile-write-report (&optional sync)
  "Write collected events to `lx/startup-profile--log-file'."
  (lx/startup-profile--ensure-origin)
  (let* ((events (reverse lx/startup-profile--events))
         (startup-total (when (boundp 'emacs-start-time)
                          (float-time (time-subtract (current-time) emacs-start-time))))
         (total (or startup-total (lx/startup-profile--elapsed)))
         (features (length features))
         (packages (when (boundp 'package-alist) (length package-alist))))
    (with-temp-buffer
      (insert (format "=== Spacemacs startup profile %s ===\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "Emacs %s\n" emacs-version))
      (insert (format "Total: %.3fs | features: %d | packages: %d\n\n"
                      total features packages))
      (insert "Phase / file                          Seconds\n")
      (insert "---------------------------------------- -------\n")
      (dolist (event events)
        (insert (format "%-40s %7.3f\n" (car event) (cdr event))))
      (insert (format "\nReport written at %s\n" lx/startup-profile--log-file))
      (write-region (point-min) (point-max) lx/startup-profile--log-file nil 'nomessage))
    (when sync
      (redisplay t))
    lx/startup-profile--log-file))

(defun lx/startup-profile-install-hooks ()
  "Register standard Spacemacs lifecycle marks."
  (lx/startup-profile-mark "profile-hooks-installed")
  (add-hook 'configuration-layer-pre-load-hook
            (lambda () (lx/startup-profile-mark "layers-pre-load")) t)
  (add-hook 'configuration-layer-post-load-hook
            (lambda () (lx/startup-profile-mark "layers-post-load")) t)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (lx/startup-profile-mark "emacs-startup-hook")
              (when (boundp 'emacs-start-time)
                (lx/startup-profile-mark
                 (format "startup-total %.3fs"
                         (float-time (time-subtract (current-time) emacs-start-time)))))
              (lx/startup-profile-write-report))
            t))

(defun lx/package-quickstart--populate-package-alist (&rest _)
  "Populate `package-alist' without activating all packages."
  (when (and dotspacemacs-enable-package-quickstart
             (not package--initialized))
    (when (fboundp 'lx/startup-profile-mark)
      (lx/startup-profile-mark "package-initialize-no-activate"))
    (package-initialize 'no-activate)))

(defun lx/package-quickstart-setup ()
  "Make package-quickstart compatible with Spacemacs package sync.

Without populating `package-alist', Spacemacs thinks packages like org and
transient are missing and reinstalls them on every startup."
  (when (fboundp 'configuration-layer//install-packages)
    (advice-remove #'configuration-layer//install-packages
                   #'lx/package-quickstart--populate-package-alist)
    (advice-add #'configuration-layer//install-packages :before
                #'lx/package-quickstart--populate-package-alist)))

(provide 'startup-profile)
