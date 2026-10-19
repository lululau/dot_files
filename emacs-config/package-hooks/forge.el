;; -*- lexical-binding: t; -*-

;; Forge 20261018.1903 autoloads bind `magit-repolist-mode-map' from
;; `magit-mode`'s after-load:
;;
;;   (with-eval-after-load 'magit-mode
;;     (keymap-set magit-repolist-mode-map "C-c C-o" #'forge-browse))
;;
;; That map is defined in magit-repos, which magit loads later.  The
;; autoload therefore signals void-variable while magit is still
;; loading, and `lx/magit-status-smart' fails.  Stub the map so the
;; autoload can run, then drop the stub before magit-repos defines the
;; real keymap.  Remove this file once forge moves the C-c C-o binding
;; into its magit-repos after-load.

(defvar magit-repolist-mode-map)

(defun lx/forge--stub-repolist-map-before-autoload (file)
  (when (string-match-p "/magit-mode\\.elc?\\'" file)
    (unless (boundp 'magit-repolist-mode-map)
      (setq magit-repolist-mode-map (make-sparse-keymap)))))

(add-hook 'after-load-functions
          #'lx/forge--stub-repolist-map-before-autoload)

(defun lx/forge--unstub-repolist-map-before-repos (feature &rest _)
  (when (and (eq feature 'magit-repos)
             (not (featurep 'magit-repos))
             (boundp 'magit-repolist-mode-map))
    (makunbound 'magit-repolist-mode-map)))

(advice-add 'require :before #'lx/forge--unstub-repolist-map-before-repos)

(with-eval-after-load 'magit-repos
  (advice-remove 'require #'lx/forge--unstub-repolist-map-before-repos)
  (remove-hook 'after-load-functions
               #'lx/forge--stub-repolist-map-before-autoload)
  (when (and (bound-and-true-p forge-add-default-bindings)
             (boundp 'magit-repolist-mode-map)
             (fboundp 'forge-browse))
    (keymap-set magit-repolist-mode-map "C-c C-o" #'forge-browse)))
