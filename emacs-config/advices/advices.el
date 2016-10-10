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

(advice-add 'run-ruby :override #'(lambda (&optional command name check-buf)
  (interactive)
  (setq command (or command (cdr (assoc inf-ruby-default-implementation
                                        inf-ruby-implementations))))
  (setq name (or name "ruby"))

  (if (not (comint-check-proc (or check-buf (if (string-match-p "\\(railsconsole\\|railsserver\\|bundleconsole\\)\\*\\*$" (or inf-ruby-buffer "")) nil inf-ruby-buffer))))
      (let ((commandlist (split-string-and-unquote command))
            (buffer (current-buffer))
            (process-environment process-environment))
        ;; http://debbugs.gnu.org/15775
        (setenv "PAGER" (executable-find "cat"))
        (set-buffer (apply 'make-comint name (car commandlist)
                           nil (cdr commandlist)))
        (inf-ruby-mode)
        (ruby-remember-ruby-buffer buffer)))
  (pop-to-buffer (setq inf-ruby-buffer (format "*%s*" name)))))

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
                                :vanilla (format "bundle exec rails server -p %d" port)) (concat "*" (projectile-project-name)  "railsserver*") (concat "*" (projectile-project-name)  "railsserver*"))
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
                                :spring "spring rails console"
                                :zeus "zeus console"
                                :vanilla "bundle exec rails console") (concat "*" (projectile-project-name)  "railsconsole*") (concat "*" (projectile-project-name)  "railsconsole*"))
           (projectile-rails-mode +1))))))

(advice-add
 'projectile-rails-spring-p
 :override
 #'(lambda ()
     (let ((root (directory-file-name (projectile-rails-root))))
       (or
        ;; Older versions
        (file-exists-p (format "%s/tmp/spring/spring.pid" root))
        ;; 0.9.2+
        (file-exists-p (format "%s/spring/%s.pid" temporary-file-directory (md5 root)))
        ;; 1.2.0+
        (let* ((path (or (getenv "XDG_RUNTIME_DIR") temporary-file-directory))
               (ruby-version (shell-command-to-string "ruby -e 'print RUBY_VERSION'"))
               (application-id (md5 (concat ruby-version root))))
          (or
           (file-exists-p (format "%s/spring/%s.pid" path application-id))
           ;; 1.5.0+
           (file-exists-p (format "%s/spring-%s/%s.pid" path (user-real-uid) application-id))))))
     ))

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

(advice-add 'bundle-command :override #'(lambda (cmd) (let ((shell-command-switch "-lc")) (async-shell-command cmd "*Bundler*"))))

(advice-add 'ace-pinyin-jump-char :after #'(lambda (&rest args)
                                      (setq avy-last-goto-entity (cons 'ace-pinyin-jump-char args))))

(advice-add 'ace-pinyin-jump-char-2 :after #'(lambda (&rest args)
                                             (setq avy-last-goto-entity (cons 'ace-pinyin-jump-char-2 args))))

(global-set-key (kbd "s-.") #'(lambda () (interactive) (eval avy-last-goto-entity)))

;; (advice-add
;;  'ggtags-eldoc-function
;;  :after-until
;;  #'(lambda (&rest args)
;;      (let* ((current-symbol (eldoc-current-symbol))
;;             (current-fnsym  (eldoc-fnsym-in-current-sexp))
;;             (doc (cond
;;                   ((null current-fnsym)
;;                    nil)
;;                   ((eq current-symbol (car current-fnsym))
;;                    (or (apply 'eldoc-get-fnsym-args-string
;;                               current-fnsym)
;;                        (eldoc-get-var-docstring current-symbol)))
;;                   (t
;;                    (or (eldoc-get-var-docstring current-symbol)
;;                        (apply 'eldoc-get-fnsym-args-string
;;                               current-fnsym))))))
;;        doc)))
