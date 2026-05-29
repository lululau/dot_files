;; -*- lexical-binding: t; -*-

;; Fix mismatched spaceline separator colors on macOS.
;;
;; powerline draws its slant/arrow/etc. separators as XPM images.  When
;; `powerline-image-apple-rgb' is non-nil it pre-converts those image colors
;; from sRGB to "Apple RGB" (an approximate gamma-1.8 transform).  This was a
;; workaround for old NS builds whose image renderer did not honor sRGB.
;;
;; On a modern Emacs NS build the image renderer DOES use sRGB -- the same
;; color space as face/text backgrounds -- so the extra conversion only darkens
;; the separators relative to the segments they join.  The result is visible
;; seams where a separator is a different shade than its neighbouring segment,
;; e.g. separator #4a3d68 against an act2 segment #5d4d7a, or separator #19191c
;; against a mode-line segment #222226.
;;
;; Disabling the conversion makes separator image colors match the segment
;; backgrounds exactly.  This must be set before powerline generates (and
;; memoizes) its separators; package-hooks load during `dotspacemacs/user-init',
;; before powerline is loaded, so the top-level `setq' wins over the `defvar'
;; default in powerline-separators.el.  The after-load reset is defensive in
;; case anything generated separators earlier.
(setq powerline-image-apple-rgb nil)

(with-eval-after-load 'spaceline
  (setq powerline-image-apple-rgb nil)
  (when (fboundp 'powerline-reset)
    (powerline-reset))
  (when (fboundp 'spaceline-compile)
    (spaceline-compile)))
