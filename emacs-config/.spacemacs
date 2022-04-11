;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Add these two lines for transparent-titlebar in emacs-plus
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq evil-want-keybinding nil)

(defun lx/system-is-linux()
  (eq system-type 'gnu/linux))

(defun lx/system-is-mac()
  (eq system-type 'darwin))

;; (if (lx/system-is-mac) (setenv "PATH" ""))

(if (file-exists-p "~/.config/secrets/secret-emacs-config.el")
    (load-file "~/.config/secrets/secret-emacs-config.el"))

(if (display-graphic-p)
    (progn
      (setq lx/spacemacs-themes '(spacemacs-dark solarized-light))
      (setq lx/spacemacs-banner "~/Documents/emacs-banners/nasa.png")
      (setq neo-theme 'icons)
      (setq enable-org-notification t)
      (setq enable-mu4e-notification t))
  (setq lx/spacemacs-themes '(spacemacs-dark solarized-light))
  (setq lx/spacemacs-banner '000)
  (setq neo-theme 'ascii)
  (setq enable-org-notification nil)
  (setq enable-mu4e-notification nil))

(if (and (lx/system-is-linux) (file-exists-p "~/liuxiang"))
    (progn
      (setq lx/conf-layer-path "~/liuxiang/spacemacs-layers"
            lx/snippets-path  "~/liuxiang/.config/emacs-config/snippets"
            lx/default-shell "~/liuxiang/local/bin/zsh"
            lx/emacs-config-init-el "~/liuxiang/.config/emacs-config/init.el"
            lx/emacs-text-objects-init-el "~/liuxiang/.config/emacs-config/text-objects/init.el"
            lx/emacs-key-bindings-init-el "~/liuxiang/.config/emacs-config/key-bindings/init.el"
            lx/emacs-vendor-init-el "~/liuxiang/.config/emacs-config/vendor/init.el"
            lx/emacs-crafts-init-el "~/liuxiang/.config/emacs-config/crafts/init.el"
            lx/org-project-file nil))
  (setq lx/conf-layer-path "~/cascode/github.com/spacemacs-layers"
        lx/snippets-path  "~/.config/emacs-config/snippets"
        lx/default-shell "/usr/bin/zsh"
        lx/emacs-config-init-el "~/.config/emacs-config/init.el"
        lx/emacs-text-objects-init-el "~/.config/emacs-config/text-objects/init.el"
        lx/emacs-key-bindings-init-el "~/.config/emacs-config/key-bindings/init.el"
        lx/emacs-vendor-init-el "~/.config/emacs-config/vendor/init.el"
        lx/emacs-crafts-init-el "~/.config/emacs-config/crafts/init.el"
        lx/org-project-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/projects.org"))

(if (lx/system-is-mac) (setq lx/default-shell "/usr/local/bin/zsh"))

(setq spacemacs-theme-comment-bg nil)

(setq helm-mm-default-match-functions '(helm-mm-exact-match helm-mm-match helm-mm-pinyin-match))

(setq use-package-inject-hooks t)

(setq company-shell--cache '(""))

;; (setq dired-quick-sort-group-directories-last ?y)

(defun dotspacemacs/layers ()

  "Configuration Layers declaration."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-elpa-timeout 60
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
     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t)
     ;; lsp
     (lsp :variables lsp-rust-server 'rust-analyzer)
     dap
     ivy helm
     neotree
     (osx :variables osx-command-as 'super)
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-show-snippets-in-popup t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-use-company-box nil
                      auto-completion-private-snippets-directory ,lx/snippets-path)
     tabnine
     better-defaults
     ;; (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
     emacs-lisp
     git
     ;; github ;; layer deprecated
     gtags
     (markdown :variables markdown-live-preview-engine 'vmd)
     pandoc
     (org :variables
          org-enable-notifications t
          org-start-notification-daemon-on-startup ,enable-org-notification
          org-enable-epub-support t
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-jira-support t
          org-jira-working-dir "~/Documents/materials/jira/"
          org-journal-dir "~/Documents/materials/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-format "%Y-%m-%d"
          ;; org-roam-directory "~/Documents/materials/roam/"
          ;; org-roam-v2-ack t
          org-projectile-file ,lx/org-project-file)
     (shell :variables
            shell-default-height 38
            shell-default-position 'bottom
            shell-default-shell 'vterm
            shell-default-term-shell ,lx/default-shell)
     syntax-checking
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'left
                      version-control-global-margin t)
     (ruby :variables
           ;; ruby-backend 'robe
           ruby-test-runner 'rspec
           ruby-enable-enh-ruby-mode nil)
     perl5
     raku
     json
     yaml
     ruby-on-rails
     projectile-bundler
     projectile-bundler-robe
     elixir
     (shell-scripts :variables shell-scripts-backend 'shell-script-mode)
     dash
     clojure
     (haskell :variables haskell-enable-hindent t haskell-completion-backend 'dante)
     emacs-lisp
     ;; evernote
     evil-commentary
     major-modes
     html
     common-lisp
     groovy
     java
     ;; (java :variables java-backend 'meghanada)
     c-c++
     (javascript  :variables javascript-disable-tern-port-files nil javascript-backend 'lsp)
     typescript
     (node :variable node-add-modules-path)
     (vue :variables vue-backend 'lsp)
     coffeescript
     react
     (python :variables python-test-runner '(pytest))
     php
     restclient
     (rust :variables rust-backend 'lsp)
     (crystal :variables crystal-backend 'lsp)
     (scala :variables scala-backend 'scala-metals)
     swift
     (kotlin :variables kotlin-lsp-jar-path "/Users/liuxiang/Documents/shared_config/kotlin-lsp-server/bin/kotlin-language-server")
     groovy
     (go :variables go-tab-width 4)
     lua
     vagrant
     docker
     protobuf
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
     pdf
     csv
     sql
     plantuml
     nginx
     vimscript
     (mu4e :variables mu4e-enable-notifications ,enable-mu4e-notification mu4e-enable-mode-line t mu4e-use-maildirs-extension t)
     confluence-lx
     ragtag
     org-yank-image
     imenu-list
     ;; rebox  ; comment out due to auto-fill-mode auto-enable of rebox
     systemd
     ;; org-jira
     lorem-ipsum-zh
     ob-rails
     ob-arql
     bm
     translator
     k8s
     spotify
     windows-scripts
     (latex :variables latex-build-command "XeLaTeX")
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(calfw calfw-org browse-at-remote ranger helm-mu
                                            jq-mode helm-dired-history go-dlv realgud-byebug
                                            dired-subtree carbon-now-sh sx daemons evil-mc
                                            proxy-mode org-super-agenda es-mode ob-mermaid ob-html-chrome
                                            ob-tmux org-tree-slide helm-tramp kubernetes-tramp emms
                                            ssh-tunnels dired-filter dired-ranger dired-narrow jdecomp
                                            code-archive dtrace-script-mode xwwp-follow-link-helm
                                            edit-indirect annotate mermaid-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(git-gutter git-gutter+ git-gutter-fringe git-gutter-fringe+
                                               chinese-pyim chinese-wbim ebuild-mode hoon-mode
                                               logcat ido evil-escape helm-xref)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-enable-server t)

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
                        (eq it 'spotify)
                        (eq (car-safe it) 'mu4e)
                        (eq (car-safe it) 'elfeed)
                        (eq it 'dash))
                  (add-to-list 'lx/conf-layers it t)
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
   dotspacemacs-startup-lists '((recents . 10) (bookmarks . 20))
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes lx/spacemacs-themes
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
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
   dotspacemacs-mode-line-theme '(spacemacs :separator slant)
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
   dotspacemacs-pretty-docs t
   dotspacemacs-use-spacelpa nil
   dotspacemacs-use-SPC-as-y t
   )
  ;; User initialization goes here

  ;; (setq configuration-layer-elpa-archives
  ;;       '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
  ;;         ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
  ;;         ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

  ;; (setq configuration-layer-elpa-archives
  ;;       '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
  ;;         ("org-cn"   . "http://elpa.emacs-china.org/org/")
  ;;         ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

  (setq configuration-layer--elpa-archives
        '(("melpa"    . "melpa.org/packages/")
          ;; ("org"      . "orgmode.org/elpa/")
          ("gnu"      . "elpa.gnu.org/packages/")))

  (load-file lx/emacs-config-init-el)

  (setq-default ruby-version-manager 'rvm)
  (setq-default ruby-enable-ruby-on-rails-support t)
  (setq evil-want-C-i-jump t)
  (add-hook 'spacemacs-buffer-mode-hook #'(lambda ()
                                            (define-key spacemacs-buffer-mode-map (kbd "s-r s-u") #'configuration-layer/update-packages)
                                            (define-key spacemacs-buffer-mode-map (kbd "s-r s-b") #'configuration-layer/rollback)))
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (load-library "autoinsert")

  (add-hook 'lsp-completion-mode-hook #'lx/reset-lsp-company-backends)

  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq powerline-default-separator 'slant)
  (load-file lx/emacs-text-objects-init-el)
  (load-file lx/emacs-key-bindings-init-el)
  (load-file lx/emacs-vendor-init-el)
  (load-file lx/emacs-crafts-init-el)
  ;; (load-file "~/.config/emacs-config/doom-themes.el")

  (setq recentf-save-file (format "~/.emacs.d/.cache/recentf.%s" server-name))

  (setq tat/window-close-delay "100000000")

  (setq tramp-terminal-type "tramp")

  (setq code-archive-dir "~/Documents/.code-archive")

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
   web-mode-attr-indent-offset 2
   javascript-indent-level 2
   js-indent-level 2
   json-reformat:indent-width 2
   coffee-tab-width 2)
  (setq neo-vc-integration nil)
  (spacemacs/set-state-faces)
  (setenv "LANG" "zh_CN.UTF-8")
  (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq magit-push-always-verify nil)
  ;; (add-hook 'smartparens-enabled-hook #'turn-off-sp-on-large-file)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)
  (add-hook 'git-commit-mode-hook 'evil-emacs-state)
  (setq helm-mode-fuzzy-match t)
  (setq helm-gtags-fuzzy-match t)
  (setq rvm--gemset-default "default")
  (setq mac-option-modifier 'meta)
  (setq frame-title-format '(:eval (lx/layouts-for-title-bar)))
  (when (lx/system-is-mac) (load-file "~/.config/secrets/paradox-github-token.el"))
  (setq helm-locate-command "~/.rvm/gems/ruby-3.1.0/bin/mfd %s %s")

  (setq edit-server-new-frame nil)
  (setq edit-server-url-major-mode-alist
        '(("docs\\.alibaba-inc\\.com" . confluence-edit-mode) ("jira\\.creditcloud\\.com" . confluence-edit-mode) ("jira\\.ktjr\\.com" . confluence-edit-mode) (".*" . markdown-mode)))
  (if (lx/system-is-mac)
      (setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
    (setq org-directory "~/.org"))
  (if (file-exists-p "~/.agenda_files")
      (setq org-agenda-files "~/.agenda_files"))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-link-search-must-match-exact-headline nil)

  (setq org-projectile-capture-template "* TODO %? %a\n")
  (add-hook 'org-capture-after-finalize-hook #'lx/delete-global-org-capture-frame)

  (setq comint-input-ring-file-name "~/.pry_history")
  (setq comint-input-ring-size 100000)
  (remove-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'after-change-major-mode-hook 'projectile-rails-on)
  (setq inf-ruby-default-implementation "pry")
  (setq inf-ruby-eval-binding "Pry.toplevel_binding")
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'inf-ruby-mode-hook #'(lambda ()
                                    (interactive)
                                    (comint-read-input-ring)
                                    (add-hook 'comint-input-filter-functions #'(lambda (text) (interactive) '(comint-write-input-ring)) nil t)))
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
  (if (lx/system-is-mac)
      (setq org-default-notes-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/capture.org")
    (setq org-default-notes-file "~/.org/notes.org"))
  (setq org-html-doctype "html5")
  (setq git-link-remote-alist
        '(("gitlab.ktjr.com"    git-link-gitlab)
          ("gitlab.creditcloud.com"    git-link-gitlab)
          ("gitlab.tiaoyin100.com"    git-link-gitlab-no-https)
          ("github.com"    git-link-github)
          ("bitbucket.org" git-link-bitbucket)
          ("gitorious.org" git-link-gitorious)
          ("gitlab.com"    git-link-gitlab)))

  (setq git-link-commit-remote-alist
    '(("gitlab.ktjr.com" git-link-commit-github)
      ("gitlab.creditcloud.com" git-link-commit-github)
      ("gitlab.tiaoyin100.com" git-link-commit-github)
      ("github.com"    git-link-commit-github)
      ("bitbucket.org" git-link-commit-bitbucket)
      ("gitorious.org" git-link-commit-gitorious)
      ("gitlab.com"    git-link-commit-github)))


  (setq org-mu4e-tmp-dir "~/tmp/mu4e")

  ;; --------- Orgcss HTML Theme for Mu4e ---------
  (setq mu4e-org-html-head (format "<link rel=\"stylesheet\" title=\"Standard\" href=\"%s.spacemacs-layers/assets/org-themes/style/org/orgcss/org.css\" type=\"text/css\" />" user-home-directory))

  ;; --------- Worg HTML Theme for Mu4e ---------
  ;; (setq mu4e-org-html-head (format "<link rel=\"stylesheet\" title=\"Standard\" href=\"%s.spacemacs-layers/assets/org-themes/style/org/worg/worg.css\" type=\"text/css\" />" user-home-directory))

  ;; --------- ReadTheOrg HTML Theme for Mu4e ---------
  ;; (setq mu4e-org-html-head (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s.spacemacs-layers/assets/org-themes/style/org/spacemacs-wide/htmlize.css\"/>\n <script src=\"%s.spacemacs-layers/assets/org-themes/js/org/spacemacs-wide/jquery.min.js\"></script>\n <script src=\"%s.spacemacs-layers/assets/org-themes/js/org/spacemacs-wide/bootstrap.min.js\"></script>\n <script src=\"%s.spacemacs-layers/assets/org-themes/js/org/spacemacs-wide/readtheorg.js\"></script>\n <link rel=\"stylesheet\" type=\"text/css\" href=\"%s.spacemacs-layers/assets/org-themes/style/org/spacemacs-wide/readtheorg.css\"/>\n <link rel=\"stylesheet\" type=\"text/css\" href=\"%s.spacemacs-layers/assets/org-themes/style/org/spacemacs-wide/font-awesome.min.css\"/>\n" user-home-directory user-home-directory user-home-directory user-home-directory user-home-directory user-home-directory ))

  ;; --------- Worg HTML Theme for Org-Mode Export ---------
  ;; (setq org-html-head "<link rel=\"stylesheet\" title=\"Standard\" href=\"http://www.hackit.fun/org-assets/css/worg/worg.css\" type=\"text/css\" />")

  ;; --------- Dakrone HTML Theme for Org-Mode Export ---------
  ;; (setq org-html-head "<link rel=\"stylesheet\" href=\"http://www.hackit.fun/org-assets/css/dakrone/org.css\" type=\"text/css\" />")

  ;; --------- OrgCSS HTML Theme for Org-Mode Export ---------
  ;; (setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>")

  ;; --------- ReadTheOrg HTML Theme for Org-Mode Export ---------
  (setq org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.hackit.fun/org-assets/css/spacemacs-wide/htmlize.css\"/>\n <script src=\"http://www.hackit.fun/org-assets/js/spacemacs-wide/jquery.min.js\"></script>\n <script src=\"http://www.hackit.fun/org-assets/js/spacemacs-wide/bootstrap.min.js\"></script>\n <script src=\"http://www.hackit.fun/org-assets/js/spacemacs-wide/readtheorg.js\"></script>\n <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.hackit.fun/org-assets/css/spacemacs-wide/readtheorg.css\"/>\n <link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.hackit.fun/org-assets/css/spacemacs-wide/font-awesome.min.css\"/>\n")

  (plist-put (cdr (assoc 'google-maps search-engine-alist)) :url "http://www.google.cn/maps/search/%s")
  (add-to-list 'search-engine-alist '(ip138 :name "ip138" :url "http://ip138.com/ips138.asp?ip=%s&action=2") t)

  (setq auto-mode-alist (append '(("\\.pryrc\\'" . ruby-mode)
                                  ("\\.rexerc\\'" . ruby-mode)
                                  ("\\.rails\\'" . ruby-mode)
                                  ("\\.arql\\'" . ruby-mode)
                                  ("\\.apib\\'" . markdown-mode)
                                  ("\\.m\\'" . objc-mode)
                                  ("\\.mm\\'" . objc-mode)
                                  ("^/tmp/zsh[a-zA-Z0-9]\\{6\\}$" . sh-mode)
                                  ("\\.es$" . es-mode)
                                  ("\\.class" . jdecomp-mode)
                                  ("\\.d$" . dtrace-script-mode)
                                  ("\\.sc$" . scala-mode)) auto-mode-alist))

  ;; (add-to-list 'magic-mode-alist '("import.+from\s+['\"]react['\"]" . rjsx-mode))

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

  (define-coding-system-alias 'UTF-8 'utf-8)
  (define-coding-system-alias 'UTF8 'utf-8)
  (define-coding-system-alias 'utf8 'utf-8)

  (setq eclim-eclipse-dirs "~/Applications/Eclipse.app" eclim-executable "~/Applications/Eclipse.app/Contents/Eclipse/eclim")
  (global-pangu-spacing-mode -1)
  (global-vi-tilde-fringe-mode -1)
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

  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
  (savehist-mode 1)

  (with-eval-after-load 'dired
    (require 'helm-dired-history))

  (add-hook 'ido-setup-hook #'(lambda () (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil)))

  (defalias 'fuck-ido-dired 'dired)
  (spacemacs/set-leader-keys "ad" 'fuck-ido-dired)

  (global-subword-mode)

  (add-to-list 'org-babel-load-languages '(elasticsearch . t))
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (add-to-list 'org-babel-load-languages '(sql . t))
  (add-to-list 'org-babel-load-languages '(jq . t))

  (autoload 'org-babel-execute:jq "ob-jq")

  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc")

  (define-key y-or-n-p-map (kbd "SPC") 'y-or-n-p-insert-y)

  (setq TeX-view-program-selection '((output-dvi . "open") (output-pdf . "open") (output-html . "open")))

  (setq python-indent-offset 4)

  (if (lx/system-is-linux)
      (setq find-ls-option '("-printf '%i  %k %M  %n %u  %g  %016s %TF %TH:%TM  %p\\n'" . "-dils")))

  (make-shell-pop-command "zsh-vterm" zsh-vterm)

  (add-hook 'post-command-hook #'lx/reset-hybrid-state-cursor-type-after-tab)
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
 '(split-width-threshold 126)
 '(persp-kill-foreign-buffer-behaviour (quote kill))
 '(org-hide-leading-stars t)
 '(org-jira-done-states '("Done" "已解决" "关闭" "Closed" "Resolved" "CLOSE" "已关闭" "解决"))
 '(org-jira-users '(("lijiajia" . "lijiajia") ("liujun" . "liujun") ("liumeina" . "liumeina") ("liuxiang" . "liuxiang") ("liuyan" . "liuyan") ("niumengluo" . "niumengluo") ("wuhaojie" . "wuhaojie") ("zhangyu" . "zhangyu")))
 '(org-super-agenda-groups
   (quote
    ((:name "IMPOARTANT !!!" :priority>= "C")
     (:name "DDHC JIRA" :category "DDHC")
     (:name "NOW !" :not (:tag "BACKLOG" :tag "LEARNINGS"))
     (:name "BACKLOG" :tag "BACKLOG")
     (:name "LEARNINGS" :tag "LEARNINGS"))))
 '(Man-notify-method (quote aggressive))
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(avy-keys (quote (97 115 100 106 107 108 119 111 112)))
 '(browse-at-remote-remote-type-domains
   (quote
    (("bitbucket.org" . "bitbucket")
     ("github.com" . "github")
     ("gitlab.ktjr.com" . "gitlab")
     ("gitlab.tiaoyin100.com" . "gitlab")
     ("gitlab.creditcloud.com" . "gitlab"))))
 '(proxy-mode-http-proxy "http://127.0.0.1:1087")
 '(proxy-mode-socks-proxy
   (quote ("Default Server" "127.0.0.1" 1086 5x5-crack)))
 '(cfw:display-calendar-holidays nil)
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(company-show-numbers t)
 '(company-idle-delay 0)
 '(es-always-pretty-print t)
 '(writeroom-bottom-divider-width 0)
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
 '(dired-subtree-ignored-regexp
   "^\\(?:\\.\\(?:bzr\\|git\\|idea\\|hg\\|s\\(?:rc\\|vn\\)\\)\\|CVS\\|MCVS\\|RCS\\|SCCS\\|_\\(?:MTN\\|darcs\\)\\|{arch}\\)$")
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(evil-emacs-state-modes
   (quote
    (org-brain-visualize-mode 5x5-mode bbdb-mode biblio-selection-mode blackbox-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bubbles-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode dun-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gomoku-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mpuz-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode pdf-outline-buffer-mode pdf-view-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode snake-mode solitaire-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode)))
 '(evil-want-Y-yank-to-eol nil)
 '(flycheck-disabled-checkers (quote (ruby-rubylint)))
 '(forge-alist
   (quote
    (("gitlab.creditcloud.com:10022" "gitlab.creditcloud.com/api/v3" "gitlab.creditcloud.com" forge-gitlab-repository)
     ("gitlab.ktjr.com:10022" "gitlab.ktjr.com/api/v4" "gitlab.ktjr.com" forge-gitlab-repository)
     ("gitlab.tiaoyin100.com" "gitlab.tiaoyin100.com/api/v3" "gitlab.tiaoyin100.com" forge-gitlab-repository)
     ("github.com" "api.github.com" "github.com" forge-github-repository)
     ("gitlab.com" "gitlab.com/api/v4" "gitlab.com" forge-gitlab-repository)
     ("salsa.debian.org" "salsa.debian.org/api/v4" "salsa.debian.org" forge-gitlab-repository)
     ("framagit.org" "framagit.org/api/v4" "framagit.org" forge-gitlab-repository)
     ("codeberg.org" "codeberg.org/api/v1" "codeberg.org" forge-gitea-repository)
     ("code.orgmode.org" "code.orgmode.org/api/v1" "code.orgmode.org" forge-gogs-repository)
     ("bitbucket.org" "api.bitbucket.org/2.0" "bitbucket.org" forge-bitbucket-repository)
     ("git.savannah.gnu.org" nil "git.savannah.gnu.org" forge-cgit*-repository)
     ("git.kernel.org" nil "git.kernel.org" forge-cgit-repository)
     ("repo.or.cz" nil "repo.or.cz" forge-repoorcz-repository)
     ("git.suckless.org" nil "git.suckless.org" forge-stagit-repository)
     ("git.sr.ht" nil "git.sr.ht" forge-srht-repository))))
 '(fringe-mode 4 nil (fringe))
 '(writeroom-width 140)
 '(writeroom-global-effects (quote (writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-vertical-scroll-bars writeroom-set-bottom-divider-width)))
 '(gh-profile-alist
   (quote
    (("github" :url "https://api.github.com" :remote-regexp "^\\(?:git@github\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")
     ;; ("kaitong" :url "https://github.ktjr.com/api/v3" :remote-regexp "^\\(?:git@github\\.ktjr\\.com:\\|\\(?:git\\|https?\\|ssh\\)://.*@?github\\.ktjr\\.com/\\)\\(.*\\)/\\(.*\\)\\(?:\\.git\\)?")
     )))
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
 '(dante-methods (quote (stack bare-ghci bare-cabal styx snack new-impure-nix new-nix nix impure-nix new-build nix-ghci mafia)))
 '(helm-M-x-fuzzy-match t)
 '(helm-ag-command-option "-U")
 '(helm-ag-ignore-patterns (quote (".cache" "GPATH" "GRTAGS" "GTAGS" "TAGS" "log")))
 '(helm-ag-use-agignore t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-dash-browser-func (quote lx/browse-url-in-safari))
 '(helm-external-programs-associations (quote (("ics" . "open"))))
 '(helm-gtags-fuzzy-match t t)
 '(helm-gtags-preselect t)
 '(helm-imenu-fuzzy-match t)
 '(helm-locate-fuzzy-match t)
 '(helm-man-format-switches "%s")
 '(helm-mu-default-search-string "(m:/INBOX or m:/\"Sent Messages\" or m:/Archive)")
 '(helm-mu-gnu-sed-program "gsed")
 '(helm-recentf-fuzzy-match t)
 '(recentf-max-saved-items 10000)
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
 '(ido-mode nil nil (ido))
 '(imenu-max-item-length nil)
 '(inf-ruby-implementations
 `(("ruby" . "irb --prompt default --noreadline -r irb/completion")
  ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
  ("rubinius" . "rbx -r irb/completion")
  ("yarv" . "irb1.9 -r irb/completion")
  ("macruby" . "macirb -r irb/completion")
  ("arql" . ,(format "%s.rvm/rubies/default/bin/ruby %s.rvm/gems/default/bin/arql" user-home-directory user-home-directory))
  ("pry" . ,(format "%s.rvm/rubies/default/bin/ruby %s.rvm/gems/default/bin/pry" user-home-directory user-home-directory))))
 '(jiralib-url "http://jira.ktjr.com")
 '(org-jira-default-jql "assignee = currentUser() AND status not in (CLOSE, closed)")
 '(js2-strict-missing-semi-warning nil)
 '(launchctl-search-path (quote ("~/.config/services/")))
 '(launchctl-filter-regex "homebrew")
 '(magit-blame-heading-format "%-20a %A %s %H")
 '(magit-diff-use-overlays nil)
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(magit-revision-show-gravatars nil)
 '(magit-section-visibility-indicator nil)
 '(markdown-command "~/bin/markdown")
 '(mu4e-attachment-dir "~/Downloads/")
 '(mu4e-headers-date-format "%Y-%m-%d")
 '(evil-want-keybinding nil)
 '(alert-default-style 'notifier)
'(mu4e-headers-fields
(quote
 ((:human-date . 12)
  (:flags . 6)
  (:mailing-list . 10)
  (:from-or-to . 22)
  (:subject))))
 '(mu4e-headers-time-format "%H:%M")
 '(ns-pop-up-frames nil)
'(org-capture-templates
(quote
 (("t" "Todo (with link to current buffer prosition)" entry
   (file "")
   "* TODO %?
  SCHEDULED: %T
  %U
  %a")
  ("c" "Todo (scheduled from right now)" entry
   (file "")
   "* TODO %?
  SCHEDULED: %T
  %U")
  ("m" "Todo (scheduled from tomorrow morning)" entry
   (file "")
   "* TODO %?
  SCHEDULED: %(org-insert-time-stamp (org-read-date t t \"+1d 10:00\") t)
  %U")
  ("g" "Todo (global capture for SPC+T)" entry
   (file "")
   "* TODO %?
  SCHEDULED: %(org-time-stamp t)
  %U")
  ("w" "Web site" entry
   (file "")
    "* %a :website:\n\n%U %?\n\n%:initial"))))
 '(org-confirm-babel-evaluate nil)
 '(org-ditaa-jar-path "/usr/local/libexec/ditaa.jar")
 '(org-export-with-sub-superscripts (quote {}))
 '(org-pandoc-options-for-latex-pdf
   `((pdf-engine . "xelatex")
     (template . ,(format "%s.spacemacs-layers/assets/pandoc-latex-templates/Heiti/default.latex" user-home-directory))))
 `(org-re-reveal-root ,(format "%s.spacemacs-layers/assets/reveal.js/" user-home-directory))
 '(org-re-reveal-theme "solarized")
'(org-src-lang-modes
(quote
 (("http" . "ob-http")
  ("ocaml" . tuareg)
  ("elisp" . emacs-lisp)
  ("ditaa" . artist)
  ("asymptote" . asy)
  ("dot" . fundamental)
  ("sqlite" . sql)
  ("calc" . fundamental)
  ("C" . c)
  ("cpp" . c++)
  ("C++" . c++)
  ("screen" . shell-script)
  ("shell" . sh)
  ("bash" . sh)
  ("arql" . ruby)
  ("rails" . ruby))))
'(package-selected-packages
(quote
 (lsp-vue ac-ispell ace-jump-helm-line ace-link ace-pinyin ace-window adaptive-wrap add-node-modules-path afternoon-theme aggressive-indent alchemist alect-themes alert all-the-icons ample-theme ample-zen-theme anaconda-mode anti-zenburn-theme anzu apropospriate-theme arduino-mode async auto-compile auto-complete auto-highlight-symbol auto-yasnippet avy badwolf-theme birds-of-paradise-plus-theme bm browse-at-remote bubbleberry-theme bundler busybee-theme calfw calfw-org carbon-now-sh cargo ccls centered-cursor-mode cherry-blossom-theme chinese-conv chruby cider cider-eval-sexp-fu clang-format clean-aindent-mode clj-refactor clojure-cheatsheet clojure-mode clojure-snippets clues-theme cmake-ide cmake-mode cmm-mode coffee-mode color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow colorsarenice-theme column-enforce-mode company company-anaconda company-c-headers company-cabal company-emacs-eclim company-ghc company-ghci company-go company-lsp company-lua company-php company-plsense company-quickhelp company-restclient company-rtags company-shell company-statistics company-tern company-web counsel counsel-dash counsel-projectile cquery csv-mode cyberpunk-theme cython-mode dactyl-mode daemons dakrone-theme dante darkburn-theme darkmine-theme darkokai-theme darktooth-theme dash dash-at-point define-word diff-hl diminish dired-subtree disaster django-theme docker docker-tramp dockerfile-mode doom-modeline doom-one-theme doom-themes dotenv-mode dracula-theme drupal-mode dumb-jump eclim edit-indirect edit-server editorconfig edn elfeed elfeed-goodies elfeed-org elfeed-web elisp-slime-nav elixir-mode emmet-mode engine-mode enh-ruby-mode ensime es-mode esh-help eshell-prompt-extras eshell-z espresso-theme eval-sexp-fu evil evil-anzu evil-args evil-cleverparens evil-commentary evil-ediff evil-escape evil-exchange evil-goggles evil-iedit-state evil-indent-plus evil-lion evil-lisp-state evil-magit evil-matchit evil-mc evil-numbers evil-org evil-search-highlight-persist evil-surround evil-terminal-cursor-changer evil-tutor evil-unimpaired evil-visual-mark-mode evil-visualstar exec-path-from-shell expand-region eyebrowse f fancy-battery farmhouse-theme fcitx feature-mode fill-column-indicator find-by-pinyin-dired firebelly-theme fish-mode flatland-theme flatui-theme flx-ido flycheck flycheck-bashate flycheck-credo flycheck-haskell flycheck-kotlin flycheck-mix flycheck-perl6 flycheck-pos-tip flycheck-rtags flycheck-rust flymd font-lock+ fuzzy gandalf-theme geeknote ggtags gh gh-md ghc gist git-commit git-link git-messenger git-timemachine gitattributes-mode gitconfig-mode github-browse-file github-clone github-search gitignore-templates gmail-message-mode gnuplot go-dlv go-eldoc go-fill-struct go-gen-test go-guru go-impl go-mode go-rename go-tag godoctor golden-ratio google-c-style google-maps google-translate gotham-theme goto-chg gradle-mode grandshell-theme groovy-imports groovy-mode gruber-darker-theme gruvbox-theme ham-mode haml-mode haskell-mode haskell-snippets hc-zenburn-theme helm helm-ag helm-c-yasnippet helm-company helm-core helm-css-scss helm-dash helm-descbinds helm-dired-history helm-dired-recent-dirs helm-flx helm-git-grep helm-gitignore helm-gtags helm-hoogle helm-make helm-mode-manager helm-mu helm-org-rifle helm-projectile helm-purpose helm-pydoc helm-rtags helm-swoop helm-themes helm-xref help-fns+ hemisu-theme heroku-theme hide-comnt highlight highlight-indentation highlight-numbers highlight-parentheses hindent hl-todo hlint-refactor ht html-to-markdown htmlize hungry-delete hy-mode hydra ibuffer-projectile ido-vertical-mode iedit imenu-list impatient-mode importmagic indent-guide inf-ruby inflections info+ inkpot-theme insert-shebang intero ir-black-theme ivy jade-mode jazz-theme jbeans-theme jq-mode js-doc js2-mode js2-refactor json-mode json-navigator julia-mode kivy-mode know-your-http-well kotlin-mode launchctl less-css-mode levenshtein light-soap-theme link-hint linum-relative live-py-mode livid-mode load-relative loc-changes lorem-ipsum lsp-go lsp-java lsp-mode lsp-ui lush-theme macrostep magit magit-gh-pulls magit-gitflow magit-popup magit-svn magithub majapahit-theme markdown-mode markdown-toc material-theme matlab-mode maven-test-mode meghanada memoize minimal-theme minitest mmm-mode moe-theme molokai-theme monochrome-theme monokai-theme move-text mu4e-alert mu4e-maildirs-extension multi-term multiple-cursors mustang-theme mvn mwim nameless naquadah-theme neotree nginx-mode niflheim-theme noctilux-theme noflet ob-coffeescript ob-elixir ob-http ob-restclient obsidian-theme occidental-theme oldlace-theme omtose-phellack-theme open-junk-file org org-brain org-bullets org-category-capture org-download org-jira org-journal org-mime org-plus-contrib org-pomodoro org-present org-projectile org-super-agenda organic-green-theme orgit origami osx-dictionary osx-trash overseer ox-gfm ox-jira ox-pandoc ox-reveal ox-twbs packed pandoc-mode pangu-spacing paradox paredit password-generator pastels-on-dark-theme pbcopy pcache pcre2el pdf-tools peg perl6-mode persp-mode phoenix-dark-mono-theme phoenix-dark-pink-theme php-auto-yasnippets php-extras phpcbf phpunit pinyinlib pip-requirements pipenv pippel pkgbuild-mode planet-theme plantuml-mode popwin powerline prettier-js professional-theme projectile projectile-rails proxy-mode pug-mode puml-mode purple-haze-theme py-isort py-yapf pyenv-mode pyim pyim-basedict pytest pyvenv qml-mode quelpa queue racer railscasts-theme rainbow-delimiters rake ranger rbenv realgud realgud-byebug rebox2 request restart-emacs restclient restclient-helm reveal-in-osx-finder reverse-theme rjsx-mode robe rspec-mode rubocop ruby-hash-syntax ruby-refactor ruby-test-mode ruby-tools rust-mode rvm s sass-mode sayid sbt-mode scad-mode scala-mode scss-mode seeing-is-believing selectric-mode seq seti-theme shell-pop simple-httpd skewer-mode slim-mode smartparens smeargle smyx-theme soft-charcoal-theme soft-morning-theme soft-stone-theme solarized-theme soothe-theme spacegray-theme spaceline spaceline-all-the-icons spacemacs-theme sparql-mode spinner sql-indent ssass-mode stan-mode stekene-theme string-inflection subatomic-theme subatomic256-theme sublime-themes sunny-day-theme swift-mode swiper sx symon systemd tagedit tango-2-theme tango-plus-theme tangotango-theme tao-theme tern test-simple thrift toc-org toml-mode toxi-theme tronesque-theme twilight-anti-bright-theme twilight-bright-theme twilight-theme ujelly-theme underwater-theme undo-tree unfill use-package uuidgen vagrant vagrant-tramp vala-mode vala-snippets vi-tilde-fringe vimrc-mode vmd-mode volatile-highlights vue-html-mode vue-mode web-beautify web-mode which-key window-numbering window-purpose winum with-editor wolfram-mode writeroom-mode ws-butler xterm-color yaml-mode yapfify yasnippet yasnippet-snippets zen-and-art-theme zenburn-theme zonokai-theme)
 ))
'(plantuml-default-exec-mode jar)
'(plantuml-jar-path "/usr/local/libexec/plantuml.jar")
'(org-plantuml-jar-path "/usr/local/libexec/plantuml.jar")
'(evil-surround-pairs-alist
  (quote
   ((40 "( " . " )")
    (91 "[ " . " ]")
    (123 "{ " . " }")
    (41 "(" . ")")
    (93 "[" . "]")
    (125 "{" . "}")
    (35 "#{" . "}")
    (98 "(" . ")")
    (66 "{" . "}")
    (62 "<" . ">")
    (116 . evil-surround-read-tag)
    (60 . evil-surround-read-tag)
    (102 . evil-surround-function)
    (115 "​" . "​"))))
'(fill-column 120)
'(projectile-completion-system (quote helm))
'(projectile-indexing-method 'hybrid)
'(projectile-git-command "git ls-files -zco")
 '(projectile-tags-file-name "NON_EXISTS_FILE")
 '(projectile-generic-command "find . -type f -print0")
 '(rake-completion-system (quote helm))
 '(realgud:nodejs-command-name "node inspect")
 '(ring-bell-function (quote ignore))
 '(rspec-primary-source-dirs (quote ("app" "lib" "src")))
 '(ruby-insert-encoding-magic-comment nil)
 '(rubocop-prefer-system-executable t)
 '(rubocop-check-command  "~/bin/rubocop --format emacs")
 '(rubocop-autocorrect-command  "~/bin/rubocop -a --format emacs")
 '(neo-window-fixed-size nil)
 '(create-lockfiles nil)
 '(python-indent-guess-indent-offset-verbose nil)
 '(dired-subtree-ignored-regexp "^\\(?:\\.\\(?:bzr\\|git\\|idea\\|hg\\|s\\(?:rc\\|vn\\)\\)\\|CVS\\|MCVS\\|RCS\\|SCCS\\|_\\(?:MTN\\|darcs\\)\\|{arch}\\)$")
 '(annotate-file "~/Documents/materials/annotates/annotations")
 '(TeX-view-program-selection (quote ((output-dvi . "open") (output-pdf . "open") (output-html . "open"))))
'(safe-local-variable-values
(quote
 ((arql-env . "lcldevb")
  (arql-env . "mddev")
  (arql-env . "mddev2")
  (arql-env . "ermasprorw")
  (arql-env . "ddhcpro")
  (arql-env . "lldevb")
  (arql-env . "insdevb")
  (arql-env . "ins247")
  (maven-trigger . "b")
  (maven-trigger . "uat")
  (vc-follow-symlinks)
  (eval text-scale-increase 3)
  (eval flycheck-mode -1 1)
  (eval org-babel-result-hide-all)
  (eval require 'org-journal)
  (encoding . utf-8)
  (elixir-enable-compilation-checking . t)
  (elixir-enable-compilation-checking)
  (org-html-head))))
 '(sh-indentation 2)
 '(org-file-apps (quote ((auto-mode . emacs) (directory . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . emacs))))
 '(sp-highlight-pair-overlay nil)
 '(spacemacs-centered-buffer-mode-fringe-color "#fdf6e4")
 '(split-height-threshold 100)
 '(spacemacs-theme-comment-bg nil)
 '(dired-filter-prefix ",f")
 '(org-babel-html-chrome-chrome-executable "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
 '(org-babel-tmux-session-prefix "")
 '(racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(jdecomp-decompiler-type 'fernflower)
 '(jdecomp-decompiler-paths (quote ((fernflower . "/Applications/IntelliJ IDEA.app/Contents/plugins/java-decompiler/lib/java-decompiler.jar"))))
 '(cargo-process--enable-rust-backtrace t)
 '(vterm-max-scrollback 10000)
 '(vterm-eval-cmds (quote (("find-file" find-file) ("message" message) ("vterm-clear-scrollback" vterm-clear-scrollback) ("lx/run-in-vterm/set-green-box-cursor" lx/run-in-vterm/set-green-box-cursor) ("lx/run-in-vterm/set-blue-bar-cursor" lx/run-in-vterm/set-blue-bar-cursor) ("update-pwd" lx/run-in-vterm/set-default-directory))))
 '(xwwp-follow-link-completion-system 'helm)
 '(helm-buffer-max-length 40)
 '(TeX-command-list (quote (("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t) ("LatexMk" "latexmk %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk") ("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode ams-tex-mode texinfo-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode context-mode) :help "Run BibTeX") ("Biber" "biber %s" TeX-run-Biber nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Run Biber") ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Generate PostScript file") ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Convert DVI file to PostScript") ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Convert DVI file to PDF with dvipdfmx") ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Convert PostScript file to PDF") ("Glossaries" "makeglossaries %s" TeX-run-command nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Run makeglossaries to create glossary file") ("Index" "makeindex %s" TeX-run-index nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Run makeindex to create index file") ("upMendex" "upmendex %s" TeX-run-index t (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Run upmendex to create index file") ("Xindy" "texindy %s" TeX-run-command nil (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode) :help "Run xindy to create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for common mistakes") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(sql-connection-alist
(quote
 (("localhost-test"
   (sql-product
    (quote mysql))
   (sql-user "root")
   (sql-database "test")
   (sql-server "")))))
 '(vc-follow-symlinks t)
 '(warning-suppress-types (quote ((comp)))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.3 :family "PingFang SC"))))
;;  '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.2 :family "PingFang SC"))))
;;  '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.15 :family "PingFang SC")))))
)
