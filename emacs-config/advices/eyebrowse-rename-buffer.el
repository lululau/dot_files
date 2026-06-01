;; -*- lexical-binding: t; -*-
;;; Fix: prevent eyebrowse from overriding persp-mode window restoration
;;
;; Root cause:
;;   When switching perspectives, two mechanisms restore window configs:
;;   1. `persp-restore-window-conf' (persp-mode)  — uses buffer OBJECTS → correct
;;   2. `spacemacs/load-eyebrowse-for-perspective' → `eyebrowse--load-window-config'
;;      — uses buffer NAME strings → stale after a rename
;;
;;   If a buffer was renamed (e.g. by uniquify: "tmp" → "tmp<jicai>") while
;;   in another persp, eyebrowse's `rename-buffer' advice only updates the
;;   current frame's data, NOT the other persp's saved workspace.  So the
;;   stored name "tmp" no longer resolves, and eyebrowse replaces it with
;;   *scratch*, overriding persp-mode's correct restoration.
;;
;; Fix:
;;   During persp activation, skip `eyebrowse--load-window-config' and
;;   instead save the current (correctly restored) window config into the
;;   eyebrose current slot.  Normal eyebrowse slot-switching within a persp
;;   is unaffected.

(defvar lx/eyebrowse-skip-window-restore nil
  "When non-nil, `eyebrowse--load-window-config' saves the current
window config into the target slot instead of restoring from it.")

;;;###autoload
(defun lx/eyebrowse--load-window-config-advise (orig-fn slot)
  "Around advice for `eyebrowse--load-window-config'.
When `lx/eyebrowse-skip-window-restore' is non-nil, don't restore
the window config from SLOT.  Instead, save the current (already
correctly restored by persp-mode) window config into SLOT."
  (if lx/eyebrowse-skip-window-restore
      ;; Don't restore — persp-mode already did it correctly.
      ;; Instead, update SLOT with the current window state so
      ;; eyebrowse data stays consistent.
      (let ((current-tag (nth 2 (assoc slot (eyebrowse--get 'window-configs)))))
        (eyebrowse--update-window-config-element
         (eyebrowse--current-window-config slot current-tag)))
    (funcall orig-fn slot)))

;;;###autoload
(defun lx/spacemacs-load-eyebrowse-for-persp-advise (orig-fn &rest args)
  "Around advice for `spacemacs/load-eyebrowse-for-perspective'.
Set `lx/eyebrowse-skip-window-restore' so that the eyebrowse load
does not override the window config that persp-mode already restored."
  (let ((lx/eyebrowse-skip-window-restore t))
    (apply orig-fn args)))

;; Install the advice after eyebrowse is loaded.
;;;###autoload
(with-eval-after-load 'eyebrowse
  (advice-add 'eyebrowse--load-window-config
              :around
              #'lx/eyebrowse--load-window-config-advise)
  (advice-add 'spacemacs/load-eyebrowse-for-perspective
              :around
              #'lx/spacemacs-load-eyebrowse-for-persp-advise))
