;; -*- lexical-binding: t; -*-

(defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                 ("insert" "chartreuse3" (bar . 2))
                                 ("emacs" "SkyBlue2" box)
                                 ("hybrid" "SkyBlue2" (bar . 2))
                                 ("replace" "chocolate" (hbar . 2))
                                 ("evilified" "LightGoldenrod3" box)
                                 ("visual" "gray" (hbar . 2))
                                 ("motion" "plum3" box)
                                 ("lisp" "HotPink1" box)
                                 ("iedit" "firebrick1" box)
                                 ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.")

(defun lx/optimize-theme-colors-and-font (&rest args)
  (when (string= "spacemacs-dark" spacemacs--cur-theme)
    (face-spec-set 'enh-ruby-op-face '((t (:foreground "#b2b2b2"))))
    (face-spec-set 'enh-ruby-string-delimiter-face '((t (:foreground "#3c95d2"))))
    (face-spec-set 'linum '((t (:background "#292b2e" :foreground "#44505c"))))
    (face-spec-set 'mode-line '((t (:background "#222226" :foreground "#b2b2b2" :box (:line-width 1 :color "#5d4d7a") :overline nil :underline nil))))
    (face-spec-set 'mode-line-inactive '((t (:background "#292b2e" :foreground "#b2b2b2" :box (:line-width 1 :color "#5d4d7a") :overline nil :underline nil))))
    (face-spec-set 'diff-hl-change '((t (:background "#69B7F0" :foreground "#00629D"))))
    (face-spec-set 'diff-hl-delete '((t (:background "#FF6E64" :foreground "#990A1B"))))
    (face-spec-set 'diff-hl-insert '((t (:background "#B4C342" :foreground "#546E00"))))
    (setq default-frame-alist (assq-delete-all 'ns-appearance default-frame-alist))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (shell-command-to-string "defaults write org.gnu.Emacs TransparentTitleBar DARK")
    (shell-command-to-string "tmux set-option -g status-style bg=colour235,fg=colour244"))

  (when (string= "solarized-light" spacemacs--cur-theme)
    (face-spec-set 'enh-ruby-op-face '((t (:foreground "#748488"))))
    (face-spec-set 'enh-ruby-string-delimiter-face '((t (:foreground "#b58901"))))
    (face-spec-set 'linum '((t (:background "#fdf6e4" :foreground "#93a1a1"))))
    (face-spec-set 'mode-line '((t (:background "#eee8d5" :foreground "#657b83" :box (:line-width 1 :color "#657b83") :overline nil :underline nil))))
    (face-spec-set 'mode-line-inactive '((t (:background "#fdf6e3" :foreground "#93a1a1" :box (:line-width 1 :color "#93a1a1") :overline nil :underline nil))))
    (face-spec-set 'diff-hl-change '((t (:background "#69B7F0" :foreground "#00629D"))))
    (face-spec-set 'diff-hl-delete '((t (:background "#FF6E64" :foreground "#990A1B"))))
    (face-spec-set 'diff-hl-insert '((t (:background "#B4C342" :foreground "#546E00"))))
    (setq default-frame-alist (assq-delete-all 'ns-appearance default-frame-alist))
    (add-to-list 'default-frame-alist '(ns-appearance . light))
    (shell-command-to-string "defaults write org.gnu.Emacs TransparentTitleBar LIGHT")
    (shell-command-to-string "tmux set-option -g status-style bg=black,fg=colour244"))

  ;; catppuccin (macchiato) — dark theme using Catppuccin Mocha palette
  ;; base=#24273a  mantle=#1e2030  crust=#181926
  ;; surface0=#363a4f  surface1=#494d64  surface2=#5b6078
  ;; text=#cad3f5  subtext0=#a5adcb  overlay0=#6e738d  overlay1=#8087a2
  ;; mauve=#c6a0f6  blue=#8aadf4  sapphire=#7dc4e4  green=#a6da95
  ;; red=#ed8796  peach=#f5a97f  yellow=#eed49f
  (when (string= "catppuccin" spacemacs--cur-theme)
    ;; Mode-line: use mantle as active bg, crust as inactive bg, surface1 as box border
    ;; This ensures powerline separators match the segment backgrounds exactly.
    (face-spec-set 'mode-line '((t (:background "#1e2030" :foreground "#cad3f5" :box (:line-width 1 :color "#494d64") :overline nil :underline nil))))
    (face-spec-set 'mode-line-inactive '((t (:background "#181926" :foreground "#6e738d" :box (:line-width 1 :color "#494d64") :overline nil :underline nil))))
    ;; Powerline segments: unify active1/2 to surface0, matching the separator source
    (face-spec-set 'powerline-active1 '((t (:background "#363a4f" :foreground "#cad3f5"))))
    (face-spec-set 'powerline-active2 '((t (:background "#363a4f" :foreground "#cad3f5"))))
    (face-spec-set 'powerline-inactive1 '((t (:background "#181926" :foreground "#6e738d"))))
    (face-spec-set 'powerline-inactive2 '((t (:background "#181926" :foreground "#6e738d"))))
    ;; Linum: base bg with surface1 fg for subtlety
    (face-spec-set 'linum '((t (:background "#24273a" :foreground "#494d64"))))
    ;; diff-hl: bright, high-contrast VCS markers
    (face-spec-set 'diff-hl-change '((t (:background "#69B7F0" :foreground "#00629D"))))
    (face-spec-set 'diff-hl-delete '((t (:background "#FF6E64" :foreground "#990A1B"))))
    (face-spec-set 'diff-hl-insert '((t (:background "#B4C342" :foreground "#546E00"))))
    ;; Magit diff: red/green backgrounds + light text (matching spacemacs-dark style)
    (face-spec-set 'magit-diff-added '((t (:background "#2a4035" :foreground "#a6da95" :extend t))))
    (face-spec-set 'magit-diff-removed '((t (:background "#402a33" :foreground "#ed8796" :extend t))))
    (face-spec-set 'magit-diff-added-highlight '((t (:background "#2d5040" :foreground "#b8e6b0" :extend t))))
    (face-spec-set 'magit-diff-removed-highlight '((t (:background "#4d2f3a" :foreground "#f5a3b0" :extend t))))
    ;; Magit section highlight: darker than base (mantle), so cursor line stands out
    (face-spec-set 'magit-section-highlight '((t (:background "#1e2030" :extend t))))
    (face-spec-set 'magit-diff-context-highlight '((t (:background "#1e2030" :foreground "#cad3f5" :extend t))))
    ;; Magit hunk heading: subtle dark background instead of catppuccin's bright surface2
    (face-spec-set 'magit-diff-hunk-heading '((t (:background "#1e2030" :foreground "#8087a2"))))
    (face-spec-set 'magit-diff-hunk-heading-highlight '((t (:background "#262a40" :foreground "#8087a2"))))
    (setq default-frame-alist (assq-delete-all 'ns-appearance default-frame-alist))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (shell-command-to-string "defaults write org.gnu.Emacs TransparentTitleBar DARK")
    (shell-command-to-string "tmux set-option -g status-style bg=colour235,fg=colour244"))

  (cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (face-spec-set (intern (format "spacemacs-%s-face" state))
                          `((t (:background ,color
                                            :foreground ,(face-background 'mode-line)
                                            :box ,(face-attribute 'mode-line :box)
                                            :inherit 'mode-line))))
           (set (intern (format "evil-%s-state-cursor" state))
                (list (when dotspacemacs-colorize-cursor-according-to-state color)
                      cursor)))
  (lx/set-monospaced-font "SauceCodePro Nerd Font Mono" "黑体-简" 14 16 14 16))

(advice-add 'spacemacs/set-state-faces :override #'lx/optimize-theme-colors-and-font)


(advice-add 'spacemacs/cycle-spacemacs-theme :after #'(lambda (&rest args)
                                                        (let ((old-frame (selected-frame))
                                                              (frame (make-frame-command)))
                                                          (set-frame-parameter frame 'width 1.0)
                                                          (set-frame-parameter frame 'height 1.0)
                                                          (set-frame-parameter frame 'top 0.0)
                                                          (set-frame-parameter frame 'left 0.0)
                                                          (select-frame frame)
                                                          (delete-frame old-frame t)
                                                          (if (bound-and-true-p ghostel-mode-map)
                                                              (define-key ghostel-mode-map (kbd "C-c C-c") #'ghostel--self-insert)))))
