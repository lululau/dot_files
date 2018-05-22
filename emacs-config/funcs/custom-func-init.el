;;; custom-func-init.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "browse-in-alfred" "browse-in-alfred.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from browse-in-alfred.el

(autoload 'lx/browse-in-alfred "browse-in-alfred" "\


\(fn PATH)" nil nil)

(autoload 'lx/browse-file-or-directory-in-alfred "browse-in-alfred" "\
Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "browse-url-in-safari" "browse-url-in-safari.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from browse-url-in-safari.el

(autoload 'lx/browse-url-in-safari "browse-url-in-safari" "\


\(fn URL)" t nil)

;;;***

;;;### (autoloads nil "byte-compile-current-buffer-file" "byte-compile-current-buffer-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from byte-compile-current-buffer-file.el

(autoload 'byte-compile-current-buffer-file "byte-compile-current-buffer-file" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "copy-org-links-at-point" "copy-org-links-at-point.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from copy-org-links-at-point.el

(autoload 'get-git-link "copy-org-links-at-point" "\


\(fn REMOTE START END)" t nil)

(autoload 'get-local-file-link "copy-org-links-at-point" "\


\(fn)" nil nil)

(autoload 'copy-org-links-at-point "copy-org-links-at-point" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "current-directory" "current-directory.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from current-directory.el

(autoload 'lx/current-directory "current-directory" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "ediff-copy-both-to-c" "ediff-copy-both-to-c.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ediff-copy-both-to-c.el

(autoload 'ediff-copy-both-to-C "ediff-copy-both-to-c" "\


\(fn)" t nil)

(autoload 'add-d-to-ediff-mode-map "ediff-copy-both-to-c" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "gantt" "gantt.el" (0 0 0 0))
;;; Generated autoloads from gantt.el

(autoload 'gantt "gantt" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "global-org-capture" "global-org-capture.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from global-org-capture.el

(autoload 'lx/make-global-org-capture "global-org-capture" "\


\(fn SYSTEM-PORCESS)" nil nil)

(autoload 'lx/delete-global-org-capture-frame "global-org-capture" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "helm-ag-searching" "helm-ag-searching.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from helm-ag-searching.el

(autoload 'lx/helm-ag-search-prefilled-pattern "helm-ag-searching" "\


\(fn KEYWORD)" nil nil)

(autoload 'lx/helm-ag-search-pry-breakpoints "helm-ag-searching" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "idea-helpers" "idea-helpers.el" (0 0 0 0))
;;; Generated autoloads from idea-helpers.el

(autoload 'lx/switch-to-layout-of-project "idea-helpers" "\


\(fn PROJECT)" nil nil)

(autoload 'lx/open-with-idea "idea-helpers" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "insert-timestamp" "insert-timestamp.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from insert-timestamp.el

(autoload 'lx/insert-timestamp "insert-timestamp" "\
Read a timestamp from the user and insert it at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "jump-to-definition-of-symbol-at-point" "jump-to-definition-of-symbol-at-point.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jump-to-definition-of-symbol-at-point.el

(autoload 'jump-to-definition-of-symbol-at-point "jump-to-definition-of-symbol-at-point" "\


\(fn)" t nil)

(autoload 'jump-to-definition-of-symbol-at-point-other-window "jump-to-definition-of-symbol-at-point" "\


\(fn)" t nil)

(autoload 'robe-jump-other-window "jump-to-definition-of-symbol-at-point" "\
Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `robe-ask'.

\(fn ARG)" t nil)

(autoload 'robe-jump-to-module-other-window "jump-to-definition-of-symbol-at-point" "\
Prompt for module, jump to a file where it has method definitions.

\(fn NAME)" t nil)

(autoload 'robe-ask-other-window "jump-to-definition-of-symbol-at-point" "\
Prompt for module, method, and jump to its definition.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "lx-set-monospaced-font" "lx-set-monospaced-font.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from lx-set-monospaced-font.el

(autoload 'lx/set-monospaced-font "lx-set-monospaced-font" "\


\(fn ENGLISH CHINESE ENGLISH-RETINA-SIZE CHINESE-RETINA-SIZE ENGLISH-NORMAL-SIZE CHINESE-NORMAL-SIZE)" nil nil)

;;;***

;;;### (autoloads nil "move-layout" "move-layout.el" (0 0 0 0))
;;; Generated autoloads from move-layout.el

(autoload 'lx/swap-list-elem "move-layout" "\


\(fn LIST A B)" nil nil)

(autoload 'lx/next-list-index "move-layout" "\


\(fn LIST IDX)" nil nil)

(autoload 'lx/previous-list-index "move-layout" "\


\(fn LIST IDX)" nil nil)

(autoload 'lx/current-layout-index "move-layout" "\


\(fn)" nil nil)

(autoload 'lx/move-layout-forward "move-layout" "\


\(fn)" t nil)

(autoload 'lx/move-layout-backward "move-layout" "\


\(fn)" t nil)

(autoload 'lx/helm-persp-replace-project "move-layout" "\


\(fn ARG)" t nil)

(autoload 'lx/persp-swith-to-buffer-project "move-layout" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "open-mail-layout-or-mu4e-main" "open-mail-layout-or-mu4e-main.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from open-mail-layout-or-mu4e-main.el

(autoload 'lx/open-mail-custom-layout-or-mu4e-main "open-mail-layout-or-mu4e-main" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "projectile" "projectile.el" (0 0 0 0))
;;; Generated autoloads from projectile.el

(autoload 'get-current-persp-project "projectile" "\


\(fn)" nil nil)

(autoload 'projectile-project-alternate-buffer "projectile" "\


\(fn)" nil nil)

(autoload 'projectile-project-switch-to-alternate-buffer "projectile" "\


\(fn)" t nil)

(autoload 'lx/find-or-create-projectile-snippet-file "projectile" "\


\(fn)" t nil)

(autoload 'lx/find-or-create-projectile-request-file "projectile" "\


\(fn)" t nil)

(autoload 'lx/find-or-create-projectile-snippet-org "projectile" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "projectile-shell-pop" "projectile-shell-pop.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from projectile-shell-pop.el

(autoload 'projectile-shell-pop "projectile-shell-pop" "\


\(fn PREFIX)" t nil)

;;;***

;;;### (autoloads nil "ruby" "ruby.el" (0 0 0 0))
;;; Generated autoloads from ruby.el

(autoload 'binding-pry-filter "ruby" "\


\(fn TEXT)" nil nil)

(autoload 'enh-ruby-toggle-block "ruby" "\


\(fn)" t nil)

(autoload 'current-line-has-pry-breakpoint-p "ruby" "\


\(fn)" nil nil)

(autoload 'delete-pry-breakpoints "ruby" "\


\(fn)" nil nil)

(autoload 'toggle-pry-breakpoint "ruby" "\


\(fn)" t nil)

(autoload 'cleanup-pry-breakpoints "ruby" "\


\(fn)" t nil)

(autoload 'lx/jump-to-code-at-point "ruby" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "set-gh-profile-current-profile" "set-gh-profile-current-profile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from set-gh-profile-current-profile.el

(autoload 'lx/set-gh-profile-current-profile "set-gh-profile-current-profile" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "set-last-dir-and-quit" "set-last-dir-and-quit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from set-last-dir-and-quit.el

(autoload 'lx/set-last-dir-and-quit "set-last-dir-and-quit" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "sudo-save" "sudo-save.el" (0 0 0 0))
;;; Generated autoloads from sudo-save.el

(autoload 'sudo-save "sudo-save" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "switch-to-previous-persp" "switch-to-previous-persp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from switch-to-previous-persp.el

(autoload 'lx/remember-previous-persp "switch-to-previous-persp" "\


\(fn &rest ARGS)" nil nil)

(autoload 'lx/switch-to-previous-perp "switch-to-previous-persp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "title-format" "title-format.el" (0 0 0 0))
;;; Generated autoloads from title-format.el

(autoload 'lx/layout-format-name "title-format" "\
Format the layout name given by NAME for display in mode-line.

\(fn NAME POS)" nil nil)

(autoload 'lx/layouts-for-title-bar "title-format" "\
Return a one liner string containing all the layout names.

\(fn)" nil nil)

(autoload 'lx/default-title-bar "title-format" "\


\(fn)" nil nil)

(autoload 'lx/toggle-title-format "title-format" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "toggle-global-mc-mode" "toggle-global-mc-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from toggle-global-mc-mode.el

(autoload 'lx/toggle-global-evil-mc-mode "toggle-global-mc-mode" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "turn-off-sp-on-large-file" "turn-off-sp-on-large-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from turn-off-sp-on-large-file.el

(autoload 'turn-off-sp-on-large-file "turn-off-sp-on-large-file" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "update-tags" "update-tags.el" (0 0 0 0))
;;; Generated autoloads from update-tags.el

(autoload 'update-tags "update-tags" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "window-move" "window-move.el" (0 0 0 0))
;;; Generated autoloads from window-move.el

(autoload 'lx/window-move-very-top "window-move" "\


\(fn)" t nil)

(autoload 'lx/window-move-very-bottom "window-move" "\


\(fn)" t nil)

(autoload 'lx/window-move-far-left "window-move" "\


\(fn)" t nil)

(autoload 'lx/window-move-far-right "window-move" "\


\(fn)" t nil)

(autoload 'lx/def-window-frame-switch-function "window-move" "\


\(fn DIRECT-ARG)" nil t)

;;;***

;;;### (autoloads nil "yank-to-end-of-line" "yank-to-end-of-line.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from yank-to-end-of-line.el

(autoload 'yank-to-end-of-line "yank-to-end-of-line" "\


\(fn)" t nil)

;;;***

(provide 'custom-func-init)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; custom-func-init.el ends here
