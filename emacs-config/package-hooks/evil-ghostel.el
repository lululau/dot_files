;; -*- lexical-binding: t; -*-

;; Background:
;; - This file is loaded synchronously from ~/.config/emacs-config/init.el very
;;   early, before Spacemacs activates packages.
;; - The real implementation lives in `crafts/evil-ghostel.el', which begins
;;   with `(require 'evil)'.
;; - When ghostel itself is being installed/updated, Spacemacs eagerly loads
;;   the freshly-installed `ghostel' feature inside
;;   `package--load-files-for-activation'. If we synchronously call
;;   `evil-ghostel-setup' inside a `with-eval-after-load' 'ghostel hook at
;;   that moment, two things can go wrong:
;;     1. `evil-ghostel-setup' is not yet autoloaded   -> void-function.
;;     2. `evil' package directory is not yet on `load-path' (it is activated
;;        after ghostel in that pass) -> (file-missing ... evil) when
;;        crafts/evil-ghostel.el tries to (require 'evil).
;;
;; Fix:
;; - Make `crafts/' visible on `load-path' and register the autoload eagerly so
;;   the symbol is always defined.
;; - Do NOT trigger any file load during package activation. Defer the actual
;;   `evil-ghostel-setup' call to `emacs-startup-hook' (runs after the whole
;;   Spacemacs init + dotspacemacs/user-config finishes), and also fire it via
;;   nested `with-eval-after-load' once both `ghostel' and `evil' are loaded.
;;   Whichever happens first wins; the function itself is idempotent enough
;;   for repeated invocation to be a no-op or harmless.

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir  (file-name-directory this-file))
       (crafts-dir (expand-file-name "../crafts" this-dir)))
  (add-to-list 'load-path crafts-dir)
  (autoload 'evil-ghostel-setup "evil-ghostel" nil nil))

(defvar lx/evil-ghostel--done nil
  "Non-nil once `evil-ghostel-setup' has been invoked successfully.")

(defun lx/evil-ghostel--maybe-setup ()
  "Run `evil-ghostel-setup' once both ghostel and evil are loaded."
  (when (and (not lx/evil-ghostel--done)
             (featurep 'ghostel)
             (featurep 'evil)
             (fboundp 'evil-ghostel-setup))
    (setq lx/evil-ghostel--done t)
    (evil-ghostel-setup)))

;; Late path: after the entire init sequence (including dotspacemacs/user-config)
;; finishes -- by this point both features are guaranteed to be on load-path if
;; they are going to be used.
(add-hook 'emacs-startup-hook #'lx/evil-ghostel--maybe-setup)

;; Eager path: if ghostel happens to be loaded later (lazy use), still make
;; sure setup runs once evil is also available.
(with-eval-after-load 'ghostel
  (with-eval-after-load 'evil
    (lx/evil-ghostel--maybe-setup)))
