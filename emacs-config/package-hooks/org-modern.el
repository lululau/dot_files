;; -*- lexical-binding: t; -*-

(with-eval-after-load 'org-modern
  (setq org-modern--table-overline '(:strike-through t))
  (face-spec-set 'org-modern-done
                 '((t :inherit (org-done org-modern-label)
                      :weight semibold
                      :foreground unspecified
                      :background unspecified
                      :inverse-video t))))
