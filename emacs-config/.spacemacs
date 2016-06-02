;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/cascode/github.com/spacemacs-layers")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     osx
     (auto-completion :variables
                      auto-completion-show-snippets-in-popup t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.config/emacs-config/snippets")
     better-defaults
     emacs-lisp
     git
     github
     gtags
     markdown
     pandoc
     (org :variables
          org-enable-github-support t)
     (shell :variables
            shell-default-height 38
            shell-default-position 'bottom
            shell-default-shell 'ansi-term
            shell-default-term-shell "/bin/zsh")
     syntax-checking
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     (ruby :variables
           ruby-enable-enh-ruby-mode t)
     yaml
     ruby-on-rails
     projectile-rails-robe
     elixir
     shell-scripts
     dash
     emacs-lisp
     ;; evernote
     evil-commentary
     extra-langs
     html
     ;; java
     javascript
     python
     restclient
     rust
     scala
     chrome
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     search-engine
     gnus
     chinese
     selectric
     ; evil-easymotion
     (elfeed :variables
             rmh-elfeed-org-files (list "~/.config/emacs-config/elfeeds.org"))
     pdf-tools
     csv
     sql
     plantuml
     nginx
     confluence
     ragtag
     org-yank-image
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(ox-reveal calfw ox-twbs browse-at-remote ranger)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(git-gutter git-gutter+ git-gutter-fringe git-gutter-fringe+ chinese-pyim chinese-wbim)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light spacemacs-dark)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "S-SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "s-,"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ack" "ag" "pt" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here

  (load-file "~/.config/emacs-config/init.el")

  (setq-default ruby-version-manager 'rvm)
  (setq-default ruby-enable-ruby-on-rails-support t)
  (setq evil-want-C-i-jump t)
  (add-hook 'spacemacs-buffer-mode-hook #'(lambda () (define-key spacemacs-buffer-mode-map (kbd "s-r") #'configuration-layer/update-packages)))

  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  (load-file "~/.config/emacs-config/text-objects/init.el")
  (load-file "~/.config/emacs-config/key-bindings/init.el")

  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  (setq neo-vc-integration nil)
  (setq diff-hl-side 'left)
  (spacemacs//set-monospaced-font "Monaco" "STHeiti" 13 16)
  (setenv "LANG" "zh_CN.UTF-8")
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (add-hook 'prog-mode-hook 'linum-mode)
  (setq magit-push-always-verify nil)
  (add-hook 'smartparens-enabled-hook #'turn-off-sp-on-large-file)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (setq helm-mode-fuzzy-match t)
  (setq helm-gtags-fuzzy-match t)
  (setq rvm--gemset-default "default")
  (defalias 'evil-insert-state 'evil-emacs-state)
  (setq mac-option-modifier 'meta)
  (setq frame-title-format '(:eval
                             (if (projectile-project-p)
                                 (concat
                                  (projectile-project-name)
                                  (if (buffer-file-name)
                                      (concat " ✈ " (substring (buffer-file-name) (length (projectile-project-root))))
                                    (concat " ✈ "(buffer-name))))
                               (if (buffer-file-name)
                                   (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
                                       (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
                                     (buffer-file-name)) (buffer-name)))))
  (load-file "~/.config/secrets/paradox-github-token.el")
  (setq helm-locate-command "/Users/liuxiang/bin/mfd %s %s")

  (setq edit-server-new-frame nil)
  (setq edit-server-url-major-mode-alist
        '(("docs\\.alibaba-inc\\.com" . confluence-edit-mode) (".*" . markdown-mode)))
  (setq org-directory "/Users/liuxiang/Library/Mobile Documents/com~apple~CloudDocs/org")
  (setq org-agenda-files (append (file-expand-wildcards "~/Library/Mobile Documents/com~apple~CloudDocs/org/**/*.org") (file-expand-wildcards  "~/Library/Mobile Documents/com~apple~CloudDocs/org/*.org")))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-mobile-inbox-for-pull "/Users/liuxiang/Library/Mobile Documents/com~apple~CloudDocs/org/flagged.org")
  (setq org-mobile-directory "/Users/liuxiang/Dropbox/应用/MobileOrg")
  (setq org-bullets-bullet-list '("𝌆" "𝌇" "𝌎" "𝌓" "𝌮"))
  (setq org-link-search-must-match-exact-headline nil)

  (setq comint-input-ring-file-name "~/.pry_history")
  (setq comint-input-ring-size 100000)
  (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'inf-ruby-mode-hook #'(lambda ()
                                    (interactive)
                                    (comint-read-input-ring)
                                    (add-hook 'comint-input-filter-functions #'(lambda (text) (interactive) '(comint-write-input-ring)) nil t)
                                    (define-key inf-ruby-mode-map (kbd "C-]") #'(lambda () (interactive)
                                                                                  (let((str (ffap-string-at-point)) path line)
                                                                                    (if (string-match "^\\(.+\\):\\([0-9]+\\).*$" str)
                                                                                        (progn (setq path (match-string 1 str))
                                                                                               (setq line (string-to-number (match-string 2 str))))
                                                                                      (setq path str)
                                                                                      (setq line 0))
                                                                                    (org-open-file path t line))))))
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
  (setq company-minimum-prefix-length 1)
  (setq org-default-notes-file "/Users/liuxiang/Library/Mobile Documents/com~apple~CloudDocs/org/notes.org")
  (setq org-html-doctype "html5")
  ;; #+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://spacemacs.org/css/readtheorg.css" />\n<script src="http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js"></script>
  ;; #+HTML_HEAD: <link rel="stylesheet" type="text/css" href="http://liuxiang.gitcafe.io/assets/css/readtheorg.css" />\n<script src="http://liuxiang.gitcafe.io/assets/js/readtheorg.js"></script>
  ;; #+HTML_HEAD: <link href="http://thomasf.github.io/solarized-css/solarized-light.min.css" rel="stylesheet"></link>
  ;; #+HTML_HEAD: <link rel="stylesheet" href="http://dakrone.github.io/org.css" type="text/css" />

  (setq org-html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"http://liuxiang.gitcafe.io/assets/css/worg.css\" type=\"text/css\" />\n<script type=\"text/javascript\" src=\"http://liuxiang.gitcafe.io/assets/js/org-toc.js\">")

  (plist-put (cdr (assoc 'google-maps search-engine-alist)) :url "http://www.google.cn/maps/search/%s")

  (setq auto-mode-alist (cons '("\\.apib\\'" . markdown-mode) auto-mode-alist))

  (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/8039/plantuml.8039.jar")

  (add-hook 'term-mode-hook #'(lambda () (interactive)
                                (define-key term-raw-map (kbd "<M-backspace>") #'term-send-raw-meta)
                                (define-key term-raw-map (kbd "s-v") #'term-paste)
                                (define-key term-raw-map (kbd "C-y") #'term-paste)
                                ))

  (add-hook 'artist-mode-hook #'(lambda () (define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)))

  (add-hook 'org-mode-hook #'(lambda ()
                               (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n\"'")
                               (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)))

  (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex")))

  ;; org-mode and appointment notifications on Mac OS 10.8+
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2013-02/msg00644.html
  (require 'appt)
  (setq appt-time-msg-list nil)    ;; clear existing appt list
  (setq appt-display-interval 8) ;; warn every 10 minutes from t - appt-message-warning-time
  (setq
   appt-message-warning-time 15  ;; send first warning 10 minutes before appointment
   appt-display-mode-line nil     ;; don't show in the modeline
   appt-display-format 'window)   ;; pass warnings to the designated window function
  (appt-activate 1)                ;; activate appointment notification
  (display-time)                   ;; activate time display

  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

  ;; set up the call to terminal-notifier
  (defvar my-notifier-path
    "/Applications/terminal-notifier.app/Contents/MacOS/terminal-notifier")
  (defun my-appt-send-notification (title msg)
    (shell-command (concat my-notifier-path " -message " msg " -title " title " -sender org.gnu.Emacs -appIcon /Users/liuxiang/.emacs.d/private/org.png")))

  ;; designate the window function for my-appt-send-notification
  (defun my-appt-display (min-to-app new-time msg)
    (my-appt-send-notification
     (format "'%s 分钟之后'" min-to-app)    ;; passed to -title in terminal-notifier call
     (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
  (setq appt-disp-window-function (function my-appt-display))
  (setq eclim-eclipse-dirs "~/Applications/Eclipse.app" eclim-executable "~/Applications/Eclipse.app/Contents/Eclipse/eclim")

  ) ;;; End of config.

;; (desktop-save-mode 1)
(defadvice split-window-right (after split-window-right-and-balance activate) (balance-windows))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(avy-keys (quote (97 115 100 106 107 108 119 111 112)))
 '(browse-at-remote/remote-type-domains
   (quote
    (("bitbucket.org" . "bitbucket")
     ("github.com" . "github")
     ("gitlab.alibaba-inc.com" . "gitlab"))))
 '(cfw:display-calendar-holidays nil)
 '(diff-hl-draw-borders nil)
 '(diff-hl-margin-mode nil)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(flycheck-disabled-checkers (quote (ruby-rubocop ruby-rubylint)))
 '(helm-gtags-fuzzy-match t)
 '(helm-gtags-preselect t)
 '(helm-imenu-fuzzy-match t)
 '(inf-ruby-implementations
   (quote
    (("ruby" . "irb --prompt default --noreadline -r irb/completion")
     ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
     ("rubinius" . "rbx -r irb/completion")
     ("yarv" . "irb1.9 -r irb/completion")
     ("macruby" . "macirb -r irb/completion")
     ("pry" . "/Users/liuxiang/.rvm/rubies/ruby-2.3.0/bin/ruby /Users/liuxiang/.rvm/gems/ruby-2.3.0/bin/pry"))))
 '(magit-blame-heading-format "%-20a %A %s")
 '(magit-diff-use-overlays nil)
 '(neo-theme (quote uni))
 '(package-archives
   (quote
    (("melpa" . "http://elpa.zilongshanren.com/melpa/")
     ("org" . "http://elpa.zilongshanren.com/org/")
     ("gnu" . "http://elpa.zilongshanren.com/gnu/"))))
 '(projectile-completion-system (quote helm))
 '(projectile-git-command "git ls-files -zco")
 '(projectile-tags-file-name "NON_EXISTS_FILE")
 '(puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8039/plantuml.8039.jar")
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values (quote ((org-html-head))))
 '(sql-connection-alist
   (quote
    (("localhost-test"
      (sql-product
       (quote mysql))
      (sql-user "root")
      (sql-database "test")
      (sql-server "")))))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.3 :family "Optima")))))
