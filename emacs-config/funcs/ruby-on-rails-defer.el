;;; ruby-on-rails-defer.el --- Defer heavy projectile-rails startup work  -*- lexical-binding: t; -*-

(defun ruby-on-rails/pre-init-projectile-rails ()
  "Replace layer init to defer projectile-rails loading (~600ms at startup)."
  (defun ruby-on-rails/init-projectile-rails ()
    (use-package projectile-rails
      :defer t
      :config
      (projectile-rails-global-mode)
      (spacemacs|diminish projectile-rails-mode " ⇋" " RoR")
      (spacemacs/set-leader-keys-for-minor-mode 'projectile-rails-mode
        "ffa" 'projectile-rails-find-locale
        "ffb" 'projectile-rails-find-job
        "ffc" 'projectile-rails-find-controller
        "ffe" 'projectile-rails-find-environment
        "fff" 'projectile-rails-find-feature
        "ffh" 'projectile-rails-find-helper
        "ffi" 'projectile-rails-find-initializer
        "ffj" 'projectile-rails-find-javascript
        "ffl" 'projectile-rails-find-lib
        "ffm" 'projectile-rails-find-model
        "ffn" 'projectile-rails-find-migration
        "ffo" 'projectile-rails-find-log
        "ffp" 'projectile-rails-find-spec
        "ffr" 'projectile-rails-find-rake-task
        "ffs" 'projectile-rails-find-stylesheet
        "fft" 'projectile-rails-find-test
        "ffu" 'projectile-rails-find-fixture
        "ffv" 'projectile-rails-find-view
        "ffw" 'projectile-rails-find-webpack
        "ffy" 'projectile-rails-find-layout
        "ff@" 'projectile-rails-find-mailer
        "fgc" 'projectile-rails-find-current-controller
        "fgd" 'projectile-rails-goto-schema
        "fge" 'projectile-rails-goto-seeds
        "fgh" 'projectile-rails-find-current-helper
        "fgj" 'projectile-rails-find-current-javascript
        "fgg" 'projectile-rails-goto-gemfile
        "fgm" 'projectile-rails-find-current-model
        "fgn" 'projectile-rails-find-current-migration
        "fgp" 'projectile-rails-find-current-spec
        "fgr" 'projectile-rails-goto-routes
        "fgs" 'projectile-rails-find-current-stylesheet
        "fgt" 'projectile-rails-find-current-test
        "fgu" 'projectile-rails-find-current-fixture
        "fgv" 'projectile-rails-find-current-view
        "fgz" 'projectile-rails-goto-spec-helper
        "fg." 'projectile-rails-goto-file-at-point
        "f:" 'projectile-rails-rake
        "fcc" 'projectile-rails-generate
        "fcd" 'projectile-rails-destroy
        "fi" 'projectile-rails-console
        "fxs" 'projectile-rails-server
        "fRx" 'projectile-rails-extract-region)
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (spacemacs/declare-prefix-for-mode mode "mf" "rails")
        (spacemacs/declare-prefix-for-mode mode "mfc" "generate/destroy")
        (spacemacs/declare-prefix-for-mode mode "mfR" "extract")
        (spacemacs/declare-prefix-for-mode mode "mfx" "server")
        (spacemacs/declare-prefix-for-mode mode "mff" "file")
        (spacemacs/declare-prefix-for-mode mode "mfg" "goto"))
      (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (run-at-time 1 nil (lambda () (require 'projectile-rails))))
              t)))

(provide 'ruby-on-rails-defer)
