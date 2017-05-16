;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun lx/system-is-linux()
  (eq system-type 'gnu/linux))

(defun lx/system-is-mac()
  (eq system-type 'darwin))

(if (lx/system-is-mac) (setenv "PATH" ""))

(if (display-graphic-p)
    (progn
      (setq lx/spacemacs-themes '(spacemacs-dark solarized-light))
      (setq lx/spacemacs-banner 'official)
      )
  (setq lx/spacemacs-themes '(spacemacs-dark solarized-light))
  (setq lx/spacemacs-banner '000))

(if (and (lx/system-is-linux) (file-exists-p "~/liuxiang"))
    (progn
      (setq lx/conf-layer-path "~/liuxiang/spacemacs-layers"
            lx/snippets-path  "~/liuxiang/.config/emacs-config/snippets"
            lx/default-shell "~/liuxiang/local/bin/zsh"
            lx/emacs-config-init-el "~/liuxiang/.config/emacs-config/init.el"
            lx/emacs-text-objects-init-el "~/liuxiang/.config/emacs-config/text-objects/init.el"
            lx/emacs-key-bindings-init-el "~/liuxiang/.config/emacs-config/key-bindings/init.el"))
  (setq lx/conf-layer-path "~/cascode/github.com/spacemacs-layers"
        lx/snippets-path  "~/.config/emacs-config/snippets"
        lx/default-shell "/bin/zsh"
        lx/emacs-config-init-el "~/.config/emacs-config/init.el"
        lx/emacs-text-objects-init-el "~/.config/emacs-config/text-objects/init.el"
        lx/emacs-key-bindings-init-el "~/.config/emacs-config/key-bindings/init.el"))


(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path (list lx/conf-layer-path)
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   `(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (osx :variables osx-command-as 'super)
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-show-snippets-in-popup t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory ,lx/snippets-path)
     better-defaults
     emacs-lisp
     git
     github
     gtags
     (markdown :variables markdown-live-preview-engine 'vmd)
     pandoc
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-projectile-file "~/Documents/org/projects.org")
     (shell :variables
            shell-default-height 38
            shell-default-position 'bottom
            shell-default-shell 'ansi-term
            shell-default-term-shell ,lx/default-shell)
     syntax-checking
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     (ruby :variables
           ruby-test-runner 'rspec
           ruby-enable-enh-ruby-mode nil)
     yaml
     ruby-on-rails
     projectile-bundler
     projectile-bundler-robe
     elixir
     shell-scripts
     dash
     clojure
     haskell
     emacs-lisp
     evernote
     evil-commentary
     extra-langs
     html
     (java :variables java-backend 'meghanada)
     c-c++
     javascript
     (python :variables python-test-runner '(nose))
     restclient
     rust
     scala
     swift
     chrome
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     search-engine
     gnus
     chinese
     selectric
     ; evil-easymotion
     ;; (elfeed :variables
     ;;         rmh-elfeed-org-files (list "~/.config/emacs-config/elfeeds.org"))
     pdf-tools
     csv
     sql
     plantuml
     nginx
     vimscript
     (mu4e :variables mu4e-enable-notifications t mu4e-enable-mode-line t)
     confluence-lx
     ragtag
     org-yank-image
     imenu-list
     ;; rebox  ; comment out due to auto-fill-mode auto-enable of rebox
     systemd
     org-jira
     lorem-ipsum-zh
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(calfw browse-at-remote ranger helm-mu jq-mode all-the-icons)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(magithub git-gutter git-gutter+ git-gutter-fringe git-gutter-fringe+ chinese-pyim chinese-wbim)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t)

  (if (lx/system-is-linux)
      (let ((lx/conf-layers '()))
        (mapc (lambda (it)
                (unless (or (eq it 'pandoc)
                        (eq it 'dash)
                        (eq it 'chrome)
                        (eq it 'gnus)
                        (eq it 'pdf-tools)
                        (eq it 'org-jira)
                        (eq it 'osx)
                        (eq (car-safe it) 'mu4e)
                        (eq (car-safe it) 'elfeed)
                        (eq it 'dash))
                  (add-to-list 'lx/conf-layers it)
                    )) dotspacemacs-configuration-layers)
        (setq-default dotspacemacs-configuration-layers lx/conf-layers))))

(defun dotspacemacs/user-init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner lx/spacemacs-banner
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes lx/spacemacs-themes
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
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
   dotspacemacs-search-tools '("ag" "ack" "pt" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-swith-to-buffer-prefers-purpose nil
   dotspacemacs-folding-method 'origami
   )
  ;; User initialization goes here

  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

  (load-file lx/emacs-config-init-el)

  (setq-default ruby-version-manager 'rvm)
  (setq-default ruby-enable-ruby-on-rails-support t)
  (setq evil-want-C-i-jump t)
  (add-hook 'spacemacs-buffer-mode-hook #'(lambda () (define-key spacemacs-buffer-mode-map (kbd "s-r") #'configuration-layer/update-packages)))

  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq powerline-default-separator 'slant)
  (load-file lx/emacs-text-objects-init-el)
  (load-file lx/emacs-key-bindings-init-el)
  ;; (load-file "~/.config/emacs-config/doom-themes.el")

  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  (which-key-mode -1)  ;; Disable whick-key-mode by default

  (setq inf-ruby-console-patterns-alist
        '((".zeus.sock" . zeus)
          (inf-ruby-console-rails-p . rails)
          ("*.gemspec" . gem)
          (inf-ruby-console-racksh-p . racksh)
          ("Gemfile" . default)
          ((lambda () (string= (expand-file-name "~/") (expand-file-name default-directory))) . default)))

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (setq neo-vc-integration nil)
  (setq diff-hl-side 'left)
  (spacemacs/set-state-faces)
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
  ;; (defalias 'evil-insert-state 'evil-emacs-state)
  (setq mac-option-modifier 'meta)
  (setq frame-title-format '(:eval (lx/layouts-for-title-bar)))
  (when (lx/system-is-mac) (load-file "~/.config/secrets/paradox-github-token.el"))
  (setq helm-locate-command "/Users/liuxiang/bin/mfd %s %s")

  (setq edit-server-new-frame nil)
  (setq edit-server-url-major-mode-alist
        '(("docs\\.alibaba-inc\\.com" . confluence-edit-mode) ("jira\\.kaitongamc\\.com" . confluence-edit-mode) (".*" . markdown-mode)))
  (setq org-directory "/Users/liuxiang/Documents/org")
  ;; (setq org-agenda-files (append (file-expand-wildcards "~/Documents/org/**/*.org") (file-expand-wildcards  "~/Documents/org/*.org")))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-mobile-inbox-for-pull "/Users/liuxiang/Documents/org/flagged.org")
  (setq org-mobile-directory "/Users/liuxiang/Dropbox/应用/MobileOrg")
  ;; (setq org-bullets-bullet-list '("𝌆" "𝌇" "𝌎" "𝌓" "𝌮"))
  (setq org-link-search-must-match-exact-headline nil)

  (setq org-projectile:capture-template "* TODO %? %a\n")
  (add-hook 'org-capture-after-finalize-hook #'lx/delete-global-org-capture-frame)

  (setq comint-input-ring-file-name "~/.pry_history")
  (setq comint-input-ring-size 100000)
  (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'after-change-major-mode-hook 'projectile-rails-on)
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
  (setq spacemacs-space-doc-modificators
        '(spacemacs//space-doc-org-indent-mode
          spacemacs//space-doc-view-mode
          spacemacs//space-doc-hide-line-numbers
          spacemacs//space-doc-emphasis-overlays
          spacemacs//space-doc-meta-tags-overlays
          spacemacs//space-doc-link-protocol
          spacemacs//space-doc-org-block-line-face-remap
          spacemacs//space-doc-org-kbd-face-remap
          spacemacs//space-doc-resize-inline-images
          spacemacs//space-doc-advice-org-do-emphasis-faces))
  (setq org-default-notes-file "/Users/liuxiang/Documents/org/notes.org")
  (setq org-html-doctype "html5")
  (setq git-link-remote-alist
        '(("gitlab.ktjr.com"    git-link-gitlab)
          ("github.com"    git-link-github)
          ("bitbucket.org" git-link-bitbucket)
          ("gitorious.org" git-link-gitorious)
          ("gitlab.com"    git-link-gitlab)))

  (setq git-link-commit-remote-alist
    '(("gitlab.ktjr.com" git-link-commit-github)
      ("github.com"    git-link-commit-github)
      ("bitbucket.org" git-link-commit-bitbucket)
      ("gitorious.org" git-link-commit-gitorious)
      ("gitlab.com"    git-link-commit-github)))

  ;; (setq org-html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"http://orgmode.org/worg/style/worg.css\" type=\"text/css\" />")
  (setq org-html-head "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />")

  (plist-put (cdr (assoc 'google-maps search-engine-alist)) :url "http://www.google.cn/maps/search/%s")
  (add-to-list 'search-engine-alist '(ip138 :name "ip138" :url "http://ip138.com/ips138.asp?ip=%s&action=2") t)

  (setq auto-mode-alist (append '(("\\.pryrc\\'" . ruby-mode)
                                  ("\\.apib\\'" . markdown-mode)
                                  ("\\.m\\'" . objc-mode)
                                  ("\\.mm\\'" . objc-mode)
                                  ("\\.sc" . scala-mode)) auto-mode-alist))

  (setq org-plantuml-jar-path "/usr/local/libexec/plantuml.jar")

  (add-hook 'term-mode-hook #'(lambda () (interactive)
                                (define-key term-raw-map (kbd "<M-backspace>") #'term-send-raw-meta)
                                (define-key term-raw-map (kbd "s-v") #'term-paste)
                                (define-key term-raw-map (kbd "C-y") #'term-paste)
                                ))

  (add-hook 'artist-mode-hook #'(lambda () (define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation)))

  (add-hook 'org-mode-hook #'(lambda ()
                               (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n\"'")
                               (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)))

  (setq mu4e-hide-index-messages t)

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
  (defvar my-notifier-path "/usr/local/bin/terminal-notifier")
  (defun my-appt-send-notification (title msg)
    (shell-command (concat my-notifier-path " -message " msg " -title " title " -sender org.gnu.Emacs -appIcon /Users/liuxiang/.emacs.d/private/org.png")))

  ;; designate the window function for my-appt-send-notification
  (defun my-appt-display (min-to-app new-time msg)
    (my-appt-send-notification
     (format "'%s 分钟之后'" min-to-app)    ;; passed to -title in terminal-notifier call
     (format "'%s'" msg)))                                ;; passed to -message in terminal-notifier call
  (setq appt-disp-window-function (function my-appt-display))

  (setq eclim-eclipse-dirs "~/Applications/Eclipse.app" eclim-executable "~/Applications/Eclipse.app/Contents/Eclipse/eclim")

  ;; (spacemacs|define-custom-layout "@Mail"
  ;;   :binding "m"
  ;;   :body
  ;;   (progn
  ;;     ;; hook to add all mu4e buffers to the layout
  ;;     (defun spacemacs-layouts/add-mu4e-buffer-to-persp ()
  ;;       (persp-add-buffer (current-buffer)
  ;;                         (persp-get-by-name
  ;;                          mu4e-spacemacs-layout-name)))
  ;;     (add-hook 'mu4e-mode-hook #'spacemacs-layouts/add-mu4e-buffer-to-persp)
  ;;     ;; Start mu4e
  ;;     (call-interactively 'mu4e)))

  ;; (require 'spaceline-all-the-icons)
  ;; (spaceline-all-the-icons-theme)
  ;; (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
  ;; (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
  ;; (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
  ;; (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode line

  ) ;;; End of config.

;; (desktop-save-mode 1)
(defadvice split-window-right (after split-window-right-and-balance activate) (balance-windows))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote aggressive))
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(avy-keys (quote (97 115 100 106 107 108 119 111 112)))
 '(browse-at-remote-remote-type-domains
   (quote
    (("bitbucket.org" . "bitbucket")
     ("github.com" . "github")
     ("gitlab.ktjr.com" . "gitlab"))))
 '(cfw:display-calendar-holidays nil)
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(custom-safe-themes
   (quote
    ("398f0209bfd642cf7a5e3e03bdc20db2822fd6746225a4bd99ccf9b26d3059d0" default)))
 '(dash-at-point-mode-alist
   (quote
    ((actionscript-mode . "actionscript")
     (arduino-mode . "arduino")
     (c++-mode . "cpp,net,boost,qt,cvcpp,cocos2dx,c,manpages")
     (c-mode . "c,glib,gl2,gl3,gl4,manpages")
     (caml-mode . "ocaml")
     (clojure-mode . "clojure")
     (coffee-mode . "coffee")
     (common-lisp-mode . "lisp")
     (cperl-mode . "perl")
     (css-mode . "css,bootstrap,foundation,less,awesome,cordova,phonegap")
     (dart-mode . "dartlang,polymerdart,angulardart")
     (elixir-mode . "elixir")
     (lisp-interaction-mode . "el,mel")
     (emacs-lisp-mode . "elisp")
     (enh-ruby-mode . "rb,rl,rspec")
     (erlang-mode . "erlang")
     (gfm-mode . "markdown")
     (go-mode . "go,godoc")
     (groovy-mode . "groovy")
     (haml-mode . "haml")
     (haskell-mode . "haskell")
     (html-mode . "html,svg,css,bootstrap,foundation,awesome,javascript,jquery,jqueryui,jquerym,angularjs,backbone,marionette,meteor,moo,prototype,ember,lodash,underscore,sencha,extjs,knockout,zepto,cordova,phonegap,yui")
     (jade-mode . "jade")
     (java-mode . "java,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc")
     (js2-mode . "javascript,backbone,angularjs")
     (js3-mode . "nodejs")
     (latex-mode . "latex")
     (less-css-mode . "less")
     (lua-mode . "lua,corona")
     (markdown-mode . "markdown")
     (nginx-mode . "nginx")
     (objc-mode . "cpp,iphoneos,macosx,appledoc,cocoapods,cocos2dx,cocos2d,cocos3d,kobold2d,sparrow,c,manpages")
     (perl-mode . "perl,manpages")
     (php-mode . "php,wordpress,drupal,zend,laravel,yii,joomla,ee,codeigniter,cakephp,phpunit,symfony,typo3,twig,smarty,phpp,html,mysql,sqlite,mongodb,psql,redis")
     (processing-mode . "processing")
     (puppet-mode . "puppet")
     (python-mode . "python3,django,twisted,sphinx,flask,tornado,sqlalchemy,numpy,scipy,saltcvp")
     (ruby-mode . "rb,rl,rspec")
     (rust-mode . "rust")
     (sass-mode . "sass,compass,bourbon,neat,css")
     (scala-mode . "scala,akka,playscala,scaladoc")
     (stylus-mode . "stylus")
     (tcl-mode . "tcl")
     (tuareg-mode . "ocaml")
     (twig-mode . "twig")
     (vim-mode . "vim")
     (yaml-mode . "chef,ansible"))))
 '(diff-hl-draw-borders nil)
 '(diff-hl-margin-mode nil)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(flycheck-disabled-checkers (quote (ruby-rubylint)))
 '(gh-profile-alist
   (quote
    (("github" :url "https://api.github.com" :remote-regexp "^\\(?:git@github\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")
     ("kaitong" :url "https://github.ktjr.com/api/v3" :remote-regexp "^\\(?:git@github\\.ktjr\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.ktjr\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?"))))
 '(gist-list-format
   (quote
    ((id "Id" 10 nil identity)
     (created "Created" 20 nil "%D %R")
     (visibility "Visibility" 10 nil
                 (lambda
                   (public)
                   (or
                    (and public "public")
                    "private")))
     (description "Description" 30 nil identity)
     (files "Files" 0 nil
            (lambda
              (files)
              (s-join ", " files))))))
 '(golden-ratio-extra-commands
   (quote
    (select-window-9 select-window-8 select-window-7 select-window-6 select-window-5 select-window-4 select-window-3 select-window-2 select-window-1 select-window-0 quit-window evil-window-move-very-bottom evil-window-move-far-right evil-window-move-far-left evil-window-move-very-top evil-window-rotate-downwards evil-window-rotate-upwards evil-window-vnew evil-window-new evil-window-prev evil-window-next evil-window-mru evil-window-top-left evil-window-bottom-right evil-window-down evil-window-up evil-window-right evil-window-left evil-window-vsplit evil-window-split evil-window-delete evil-avy-goto-line evil-avy-goto-word-or-subword-1 buf-move-down buf-move-up buf-move-right buf-move-left avy-pop-mark ace-maximize-window ace-swap-window ace-select-window ace-delete-window ace-window windmove-left windmove-right windmove-down windmove-up lx/window-up-fallback-to-switch-frame lx/window-down-fallback-to-switch-frame)))
 '(helm-M-x-fuzzy-match t)
 '(helm-ag-command-option "-U")
 '(helm-ag-ignore-patterns (quote (".cache" "GPATH" "GRTAGS" "GTAGS" "TAGS" "log")))
 '(helm-ag-use-agignore t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-dash-browser-func (quote lx/browse-url-in-safari))
 '(helm-gtags-fuzzy-match t t)
 '(helm-gtags-preselect t)
 '(helm-imenu-fuzzy-match t)
 '(helm-locate-fuzzy-match t)
 '(helm-man-format-switches "%s")
 '(helm-mu-default-search-string "(m:/INBOX or m:/\"Sent Messages\" or m:/Archive)")
 '(helm-mu-gnu-sed-program "gsed")
 '(helm-recentf-fuzzy-match t)
 '(helm-source-projectile-projects-actions
   (quote
    (("Switch to project" .
      #[257 "\301\302!)\207"
            [projectile-completion-system helm projectile-switch-project-by-name]
            3 "

(fn PROJECT)"])
     ("Open Dired in project's directory `C-d'" . dired)
     ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
     ("Switch to Eshell `M-e'" . helm-projectile-switch-to-eshell)
     ("Grep in projects `C-s'" . helm-projectile-grep)
     ("Compile project `M-c'. With C-u, new compile command" . helm-projectile-compile-project)
     ("Remove project(s) from project list `M-D'" . helm-projectile-remove-known-project)
     ("Switch to last visited buffer in project `<s-return>'" . projectile-switch-to-project-last-buffer))))
 '(imenu-max-item-length nil)
 '(inf-ruby-implementations
(quote
 (("ruby" . "irb --prompt default --noreadline -r irb/completion")
  ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
  ("rubinius" . "rbx -r irb/completion")
  ("yarv" . "irb1.9 -r irb/completion")
  ("macruby" . "macirb -r irb/completion")
  ("pry" . "/Users/liuxiang/.rvm/rubies/ruby-2.4.0/bin/ruby /Users/liuxiang/.rvm/gems/ruby-2.4.0/bin/pry"))))
 '(jiralib-url "http://jira.kaitongamc.com")
 '(magit-blame-heading-format "%-20a %A %s %H")
 '(magit-diff-use-overlays nil)
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(markdown-command "/Users/liuxiang/bin/markdown")
 '(mu4e-attachment-dir "/Users/liuxiang/Downloads/")
 '(mu4e-headers-date-format "%Y-%m-%d")
'(mu4e-headers-fields
(quote
 ((:human-date . 12)
  (:flags . 6)
  (:mailing-list . 10)
  (:from-or-to . 22)
  (:subject))))
 '(mu4e-headers-time-format "%H:%M")
 '(neo-theme (quote icons))
 '(ns-pop-up-frames nil)
 '(org-agenda-files "~/.agenda_files")
 '(org-bullets-bullet-list (quote ("" "★" "☆" "⦿" "✪" "✬" "❖" "✦" "✧")))
'(org-capture-templates
(quote
 (("t" "Todo" entry
   (file+headline "" "Tasks")
   "* TODO %?
  %u
  %a")
  ("g" "Todo" entry
   (file+headline "" "Tasks")
   "* TODO %?
  %u"))))
 '(org-confirm-babel-evaluate nil)
 '(org-export-with-sub-superscripts (quote {}))
'(org-pandoc-options-for-latex-pdf
(quote
 ((latex-engine . "xelatex")
  (template . "/Users/liuxiang/Documents/org/pandoc-latex-templates/Heiti/default.latex"))))
 '(org-reveal-root "./reveal.js/")
 '(org-reveal-theme "blood")
'(package-selected-packages
(quote
 (realgud test-simple loc-changes load-relative evil-terminal-cursor-changer groovy-mode disaster memoize winum eclim pinyinlib diminish string-inflection spaceline-all-the-icons meghanada symon ham-mode html-to-markdown flymd skewer-mode powerline google-maps pcre2el vimrc-mode dactyl-mode seq spinner vmd-mode restclient-helm ob-restclient company-restclient know-your-http-well highlight gh origami ox-jira org-jira geeknote jq-mode multiple-cursors inf-ruby helm-purpose insert-shebang hide-comnt window-purpose inflections plantuml-mode doom-themes all-the-icons font-lock+ doom-one-theme minitest async zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme colorsarenice-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme systemd rebox2 imenu-list pug-mode cmake-mode undo-tree s elixir-mode iedit clojure-snippets clj-refactor edn paredit peg cider-eval-sexp-fu cider queue clojure-mode intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode company-cabal cmm-mode clang-format ht simple-httpd rake osx-dictionary elfeed js2-mode anaconda-mode hydra alert auto-complete company yapfify request py-isort dumb-jump git-commit dash projectile evil rust-mode tern cargo anzu f sbt-mode smartparens scala-mode with-editor helm-core markdown-mode org avy flycheck helm magit yasnippet magit-popup evil-unimpaired pcache yaml-mode xterm-color ws-butler wolfram-mode window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org thrift tagedit stan-mode sql-indent spacemacs-theme spaceline solarized-theme smeargle slim-mode shell-pop selectric-mode scss-mode scad-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reveal-in-osx-finder restclient restart-emacs rbenv ranger rainbow-delimiters racer quelpa qml-mode pyvenv pytest pyenv-mode py-yapf puml-mode projectile-rails pip-requirements persp-mode pdf-tools pbcopy paradox pangu-spacing pandoc-mode ox-twbs ox-reveal ox-pandoc ox-gfm osx-trash orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-http nginx-mode neotree mwim multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode matlab-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl julia-mode json-mode js2-refactor js-doc jade-mode info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mu helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gmail-message-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags flycheck-rust flycheck-pos-tip flycheck-mix flx-ido fish-mode find-by-pinyin-dired fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime enh-ruby-mode engine-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies edit-server diff-hl define-word dash-at-point cython-mode csv-mode company-web company-tern company-statistics company-shell company-quickhelp company-anaconda column-enforce-mode coffee-mode clean-aindent-mode chruby calfw bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile arduino-mode alchemist aggressive-indent adaptive-wrap ace-window ace-pinyin ace-link ace-jump-helm-line ac-ispell)))
 '(plantuml-jar-path "/usr/local/Cellar/plantuml/Current/libexec/plantuml.jar")
 '(projectile-completion-system (quote helm))
 '(projectile-git-command "git ls-files -zco")
 '(projectile-tags-file-name "NON_EXISTS_FILE")
 '(rake-completion-system (quote helm))
 '(ring-bell-function (quote ignore))
 '(rspec-primary-source-dirs (quote ("app" "lib" "src")))
 '(ruby-insert-encoding-magic-comment nil)
'(safe-local-variable-values
(quote
 ((encoding . utf-8)
  (elixir-enable-compilation-checking . t)
  (elixir-enable-compilation-checking)
  (org-html-head))))
 '(sp-highlight-pair-overlay nil)
 '(spacemacs-centered-buffer-mode-fringe-color "#fdf6e4")
 '(spacemacs-theme-comment-bg nil)
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
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.3 :family "PingFang SC"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.2 :family "PingFang SC"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.15 :family "PingFang SC")))))
)
