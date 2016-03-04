
;;;;; package hacks;;;;;;

(with-eval-after-load 'helm
  (define-key helm-map (kbd "s-l") 'ace-jump-helm-line))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-r") 'helm-company))

(defun projectile-switch-to-project-last-buffer (project)
  (interactive)
  (let* ((default-directory project)
        (buffers (projectile-project-buffers)))
    (if (cadr buffers)
        (switch-to-buffer (cadr buffers))
      (let ((projectile-completion-system 'helm))
        (projectile-switch-project-by-name project)))))

(with-eval-after-load "helm-projectile"
  (setq helm-source-projectile-projects
    (helm-build-in-buffer-source "Projectile projects"
      :data (lambda ()
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects))
      :fuzzy-match helm-projectile-fuzzy-match
      :keymap helm-projectile-projects-map
      :mode-line helm-read-file-name-mode-line-string
      :action '(("Switch to project" .
                (lambda (project)
                  (let ((projectile-completion-system 'helm))
                    (projectile-switch-project-by-name project))))
                ("Switch to last visited buffer in project `<s-return>'" . projectile-switch-to-project-last-buffer)
                ("Open Dired in project's directory `C-d'" . dired)
                ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
                ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
                ("Grep in projects `C-s'" . helm-projectile-grep)
                ("Compile project `M-c'. With C-u, new compile command"
                . helm-projectile-compile-project)
                ("Remove project(s) `M-D'" . helm-projectile-remove-known-project))))

  (helm-projectile-define-key helm-projectile-projects-map (kbd "<s-return>") #'projectile-switch-to-project-last-buffer)
  )

(with-eval-after-load "python"
  (defun python-shell-send-line (&optional send-main)
    "Send the entire buffer to inferior Python process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== '__main__':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument."
    (interactive "P")
    (save-restriction
      (widen)
      (python-shell-send-region (line-beginning-position) (line-end-position) send-main)))

  (defun python-shell-send-line-switch ()
    (interactive)
    (python-shell-send-line)
    (python-shell-switch-to-shell)
    (evil-insert-state))

  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "sL" 'python-shell-send-line-switch
    "sl" 'python-shell-send-line)
  )

(with-eval-after-load "neotree"
  (defcustom neo-theme 'classic
    "*The tree style to display.
`classic' use icon to display, it only it suitable for GUI mode.
`ascii' is the simplest style, it will use +/- to display the fold state,
it suitable for terminal.
`arrow' use unicode arrow.
`nerd' use the nerdtree indentation mode and arrow.
`uni' use unicode characters for arrow, folder and file."
    :group 'neotree
    :type '(choice (const classic)
                   (const ascii)
                   (const arrow)
                   (const nerd)
                   (const uni)))

(defun neo-buffer--insert-fold-symbol (name)
  "Write icon by NAME, the icon style affected by neo-theme.
`open' write opened folder icon.
`close' write closed folder icon.
`leaf' write leaf icon."
  (let ((n-insert-image (lambda (n)
                          (insert-image (neo-buffer--get-icon n))))
        (n-insert-symbol (lambda (n)
                           (neo-buffer--insert-with-face
                            n 'neo-expand-btn-face))))
    (cond
     ((and window-system (equal neo-theme 'classic))
      (or (and (equal name 'open)  (funcall n-insert-image "open"))
          (and (equal name 'close) (funcall n-insert-image "close"))
          (and (equal name 'leaf)  (funcall n-insert-image "leaf"))))
     ((equal neo-theme 'arrow)
      (or (and (equal name 'open)  (funcall n-insert-symbol "▾"))
          (and (equal name 'close) (funcall n-insert-symbol "▸"))))
     ((equal neo-theme 'nerd)
      (or (and (equal name 'open)  (funcall n-insert-symbol "▾ "))
          (and (equal name 'close) (funcall n-insert-symbol "▸ "))
          (and (equal name 'leaf)  (funcall n-insert-symbol "  "))))
     ((and window-system (equal neo-theme 'uni))
      (or (and (equal name 'open)  (funcall n-insert-symbol "  "))
          (and (equal name 'close) (funcall n-insert-symbol "  "))
          (and (equal name 'leaf)  (funcall n-insert-symbol "   "))))
     (t
      (or (and (equal name 'open)  (funcall n-insert-symbol "-"))
          (and (equal name 'close) (funcall n-insert-symbol "+"))))))))

(with-eval-after-load "browse-at-remote"
  (defun browse-at-remote/parse-git-prefixed (remote-url)
    "Extract domain and slug from REMOTE-URL like git@..."
    (cdr (s-match "git@\\([a-z.-]+\\):\\([a-z0-9_.-]+/[a-z0-9_.-]+?\\)\\(?:\.git\\)?$" remote-url)))

  (defun browse-at-remote/get-url-from-remote (remote-url)
    "Return (DOMAIN . URL) from REMOTE-URL."
    (let* ((parsed
            (cond
            ((s-starts-with? "git" remote-url) (browse-at-remote/parse-git-prefixed remote-url))
            ((s-starts-with? "http" remote-url) (browse-at-remote/parse-https-prefixed remote-url))))
          (proto
            (if (s-starts-with? "https:" remote-url) "https" "http"))
          (domain (car parsed))
          (slug (nth 1 parsed)))
      (cons domain (format "%s://%s/%s" proto domain slug)))))

(with-eval-after-load "helm-files"
  (defun helm-substitute-in-filename (fname)
    (cond ((and ffap-url-regexp
                (string-match-p ffap-url-regexp fname))
          fname)
          ((and (file-remote-p fname)
                helm-substitute-in-filename-stay-on-remote)
          (let ((sub (substitute-in-file-name fname)))
            (if (file-directory-p sub)
                sub (replace-regexp-in-string "/\\'" "" sub))))
          (t
          (with-temp-buffer
            (insert fname)
            (goto-char (point-min))
            (skip-chars-forward "/") ;; Avoid infloop in UNC paths Issue #424
            (if (re-search-forward "~/\\|//\\|/[[:alpha:]]:/" nil t)
                (let ((match (match-string 0)))
                  (goto-char (if (or (string= match "//")
                                      (string-match-p "/[[:alpha:]]:/" match))
                                  (1+ (match-beginning 0))
                                  (match-beginning 0)))
                  (buffer-substring-no-properties (point) (point-at-eol)))
                fname))))))

;;;;;;; Advices ;;;;;;;;

(advice-add 'magit-blame-format-time-string :override #'(lambda (format time tz)
                                                          (format-time-string format (seconds-to-time time))))

(advice-add 'helm-gtags--exec-global-command :override #'(lambda (type input &optional detail)
  (let ((args (helm-gtags--construct-command type input)))
    (helm-gtags--find-tag-directory)
    (helm-gtags--save-current-context)
    (let ((buf-coding buffer-file-coding-system) (current-buffer-filename (buffer-file-name)))
      (with-current-buffer (helm-candidate-buffer 'global)
        (let ((default-directory (helm-gtags--base-directory))
              (input (car (last args)))
              (coding-system-for-read buf-coding)
              (coding-system-for-write buf-coding))
          (if (zerop (apply 'process-file "global" nil '(t nil) nil args))
              (progn (let ((global-out (buffer-string)))
                       (erase-buffer)
                       (insert (s-join "\n" (cdr (--map (car it) (--sort (or (s-suffix? (cadr it) current-buffer-filename) (string< (car it) (car other))) (--map (list it (substring it 0 (string-match ":[0-9]+:" it))) (split-string global-out "\n")))))))))
            (error (format "%s: not found" input)))
          (when detail
            (helm-gtags--show-detail))))))))

(advice-add 'projectile-completing-read :override #'(lambda (prompt choices &optional initial-input)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'ido)
      (ido-completing-read prompt choices nil nil initial-input))
     ((eq projectile-completion-system 'default)
      (completing-read prompt choices nil nil initial-input))
     ((eq projectile-completion-system 'helm)
      (if (fboundp 'helm-comp-read)
          (helm-comp-read prompt choices
                          :initial-input initial-input
                          :fuzzy t
                          :candidates-in-buffer t
                          :must-match 'confirm)
        (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
     ((eq projectile-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))
        (user-error "Please install grizzl from \
https://github.com/d11wtq/grizzl")))
     ((eq projectile-completion-system 'ivy)
      (if (fboundp 'ivy-completing-read)
          (ivy-completing-read prompt choices nil nil initial-input)
        (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
     (t (funcall projectile-completion-system prompt choices))))))

(advice-add 'projectile-rails-server :override #'(lambda (port)
      (interactive "P")
      (require 'inf-ruby)
      (if (not port) (setq port 3000))
      (projectile-rails-with-root
       (progn
         (if (not (comint-check-proc inf-ruby-buffer)) (rvm-activate-corresponding-ruby))
         (with-current-buffer (run-ruby
                               (projectile-rails-with-preloader
                                :spring "bundle exec spring rails server"
                                :zeus "zeus server"
                                :vanilla (format "bundle exec rails server -p %d" port)) (concat "*" (projectile-project-name)  "railsserver*"))
           (projectile-rails-mode +1)
           (add-hook 'comint-output-filter-functions 'binding-pry-filter nil t))))))

(advice-add 'projectile-rails-console :override #'(lambda ()
      (interactive)
      (require 'inf-ruby)
      (projectile-rails-with-root
       (progn
         (if (not (comint-check-proc inf-ruby-buffer)) (rvm-activate-corresponding-ruby))
         (with-current-buffer (run-ruby
                               (projectile-rails-with-preloader
                                :spring "bundle exec spring rails console"
                                :zeus "zeus console"
                                :vanilla "bundle exec rails console") (concat "*" (projectile-project-name)  "railsconsole*"))
           (projectile-rails-mode +1))))))

(advice-add 'evil-refresh-cursor :override #'(lambda (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
STATE defaults to the current state.
BUFFER defaults to the current buffer."
  (when (and (boundp 'evil-local-mode) evil-local-mode (get-buffer-window (or buffer (current-buffer))))
    (let* ((state (or state evil-state 'normal))
           (default (or evil-default-cursor t))
           (cursor (evil-state-property state :cursor t))
           (color (or (and (stringp cursor) cursor)
                      (and (listp cursor)
                           (evil-member-if #'stringp cursor))
                      (frame-parameter nil 'cursor-color))))
      (with-current-buffer (or buffer (current-buffer))
        ;; if both STATE and `evil-default-cursor'
        ;; specify a color, don't set it twice
        (when (and color (listp default))
          (setq default (evil-filter-list #'stringp default)))
        (evil-set-cursor default)
        (evil-set-cursor cursor))))))

(advice-add 'ace-pinyin-jump-char :after #'(lambda (&rest args)
                                      (setq avy-last-goto-entity (cons 'ace-pinyin-jump-char args))))

(advice-add 'ace-pinyin-jump-char-2 :after #'(lambda (&rest args)
                                             (setq avy-last-goto-entity (cons 'ace-pinyin-jump-char-2 args))))

(global-set-key (kbd "s-.") #'(lambda () (interactive) (eval avy-last-goto-entity)))
