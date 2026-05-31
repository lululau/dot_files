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
    ;; 用 subtext1 (#b8c0e0) 替代默认 text (#cad3f5)，降低亮度更护眼
    (face-spec-set 'default '((t (:foreground "#b8c0e0"))))
    ;; Font-lock: add bold like spacemacs-dark (catppuccin only sets foreground)
    (face-spec-set 'font-lock-function-name-face '((t (:foreground "#8aadf4" :inherit bold))))
    (face-spec-set 'font-lock-keyword-face '((t (:foreground "#c6a0f6" :inherit bold))))
    (face-spec-set 'font-lock-type-face '((t (:foreground "#eed49f" :inherit bold))))
    (face-spec-set 'minibuffer-prompt '((t (:foreground "#a5adcb" :inherit bold))))
    ;; Org: DONE 标题文字用 green 与 DONE 关键字背景一致
    (face-spec-set 'org-headline-done '((t (:foreground "#a6da95"))))
    ;; Mode-line: use mantle as active bg, crust as inactive bg, surface1 as box border
    ;; This ensures powerline separators match the segment backgrounds exactly.
    (face-spec-set 'mode-line '((t (:background "#1e2030" :foreground "#cad3f5" :box (:line-width 1 :color "#494d64") :overline nil :underline nil))))
    (face-spec-set 'mode-line-inactive '((t (:background "#181926" :foreground "#6e738d" :box (:line-width 1 :color "#494d64") :overline nil :underline nil))))
    ;; Powerline segments: unify active1/2 to surface0, matching the separator source
    (face-spec-set 'powerline-active1 '((t (:background "#363a4f" :foreground "#cad3f5"))))
    (face-spec-set 'powerline-active2 '((t (:background "#363a4f" :foreground "#cad3f5"))))
    (face-spec-set 'powerline-inactive1 '((t (:background "#181926" :foreground "#6e738d"))))
    (face-spec-set 'powerline-inactive2 '((t (:background "#181926" :foreground "#6e738d"))))
    ;; Avy: hardcoded high-contrast colors for maximum jump-target visibility
    ;; (same as spacemacs-dark override — intentionally theme-independent)
    (face-spec-set 'avy-lead-face   '((t (:foreground "white" :background "#e52b50"))))
    (face-spec-set 'avy-lead-face-0 '((t (:foreground "white" :background "#4f57f9"))))
    (face-spec-set 'avy-lead-face-1 '((t (:foreground "white" :background "gray"))))
    (face-spec-set 'avy-lead-face-2 '((t (:foreground "white" :background "#f86bf3"))))
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
    ;; Magit blame: surface0 bg with catppuccin accent fg
    (face-spec-set 'magit-blame-heading '((t (:background "#363a4f" :foreground "#a6da95" :extend t))))
    (face-spec-set 'magit-blame-summary '((t (:background "#363a4f" :foreground "#eed49f" :extend t))))
    (face-spec-set 'magit-blame-culprit '((t (:background "#363a4f" :foreground "#eed49f"))))
    (face-spec-set 'magit-blame-name '((t (:background "#363a4f" :foreground "#eed49f"))))
    (face-spec-set 'magit-blame-time '((t (:background "#363a4f" :foreground "#a6da95"))))
    ;; Magit branches: tinted backgrounds with catppuccin accent colors
    (face-spec-set 'magit-branch '((t (:foreground "#c6a0f6" :inherit bold))))
    (face-spec-set 'magit-branch-current '((t (:background "#2a3548" :foreground "#8aadf4" :inherit bold :box t))))
    (face-spec-set 'magit-branch-local '((t (:background "#2a3548" :foreground "#8aadf4" :inherit bold))))
    ;; Magit reflog: pure accent foregrounds
    (face-spec-set 'magit-reflog-amend '((t (:foreground "#c6a0f6"))))
    (face-spec-set 'magit-reflog-checkout '((t (:foreground "#8aadf4"))))
    (face-spec-set 'magit-reflog-cherry-pick '((t (:foreground "#a6da95"))))
    (face-spec-set 'magit-reflog-commit '((t (:foreground "#a6da95"))))
    (face-spec-set 'magit-reflog-merge '((t (:foreground "#a6da95"))))
    (face-spec-set 'magit-reflog-other '((t (:foreground "#8bd5ca"))))
    (face-spec-set 'magit-reflog-rebase '((t (:foreground "#c6a0f6"))))
    (face-spec-set 'magit-reflog-remote '((t (:foreground "#8bd5ca"))))
    (face-spec-set 'magit-reflog-reset '((t (:foreground "#ed8796"))))
    ;; Magit log labels: bright accent bg + base fg (like spacemacs-dark style)
    (face-spec-set 'magit-log-head-label-head '((t (:background "#eed49f" :foreground "#24273a" :inherit bold))))
    (face-spec-set 'magit-log-head-label-local '((t (:background "#8aadf4" :foreground "#24273a" :inherit bold))))
    (face-spec-set 'magit-log-head-label-remote '((t (:background "#a6da95" :foreground "#24273a" :inherit bold))))
    (face-spec-set 'magit-log-head-label-tags '((t (:background "#c6a0f6" :foreground "#24273a" :inherit bold))))
    (face-spec-set 'magit-log-head-label-wip '((t (:background "#8bd5ca" :foreground "#24273a" :inherit bold))))
    (face-spec-set 'magit-log-sha1 '((t (:foreground "#8bd5ca"))))
    ;; Magit hash: lavender instead of subtext0 (too close to default text)
    (face-spec-set 'magit-hash '((t (:foreground "#91acee"))))
    ;; Magit misc
    (face-spec-set 'magit-diff-file-heading-highlight '((t (:background "#363a4f" :foreground "#8087a2"))))
    (face-spec-set 'magit-item-highlight '((t (:background "#1e2030" :extend t))))
    (face-spec-set 'magit-section-title '((t (:background "#24273a" :foreground "#8aadf4" :inherit bold))))
    ;; Spaceline: catppuccin doesn't define these spacemacs-specific faces
    (face-spec-set 'spaceline-python-venv '((t (:foreground "#c6a0f6"))))
    (face-spec-set 'spacemacs-transient-state-title-face '((t (:background unspecified :foreground "#c6a0f6" :box nil :inherit bold))))
    ;; Mode-line complement
    (face-spec-set 'mode-line-buffer-id '((t (:inherit bold :foreground "#c6a0f6"))))
    ;; Flycheck fringe: catppuccin doesn't define fringe indicator faces
    (face-spec-set 'flycheck-fringe-error '((t (:foreground "#ed8796" :inherit bold))))
    (face-spec-set 'flycheck-fringe-warning '((t (:foreground "#f5a97f" :inherit bold))))
    (face-spec-set 'flycheck-fringe-info '((t (:foreground "#8aadf4" :inherit bold))))
    (face-spec-set 'flycheck-error-list-checker-name '((t (:foreground "#8aadf4"))))
    ;; Smartparens: catppuccin doesn't define
    (face-spec-set 'sp-pair-overlay-face '((t (:background "#363a4f" :foreground unspecified))))
    (face-spec-set 'sp-show-pair-match-face '((t (:foreground "#a6da95" :inherit bold :underline t))))
    ;; Org-mode: faces catppuccin doesn't define
    (face-spec-set 'org-agenda-clocking '((t (:background "#363a4f" :foreground "#c6a0f6"))))
    (face-spec-set 'org-clock-overlay '((t (:foreground "#c6a0f6"))))
    (face-spec-set 'org-date-selected '((t (:background "#c6a0f6" :foreground "#24273a"))))
    (face-spec-set 'org-kbd '((t (:inherit region :foreground "#cad3f5" :box (:line-width 1 :style released-button)))))
    (face-spec-set 'org-time-grid '((t (:foreground "#8bd5ca"))))
    (face-spec-set 'org-verse '((t (:inherit org-block :slant italic))))
    ;; Helm: faces catppuccin doesn't define
    (face-spec-set 'helm-source-header '((t (:background "#c6a0f6" :foreground "#181926" :inherit bold))))
    (face-spec-set 'helm-match '((t (:background "#363a4f" :foreground "#8aadf4"))))
    (face-spec-set 'helm-match-item '((t (:background "#363a4f" :foreground "#8aadf4"))))
    (face-spec-set 'helm-selection-line '((t (:background "#1e2030"))))
    (face-spec-set 'helm-buffer-directory '((t (:foreground "#cad3f5" :background "#24273a"))))
    (face-spec-set 'helm-buffer-file '((t (:foreground "#cad3f5" :background "#24273a"))))
    (face-spec-set 'helm-header-line-left-margin '((t (:foreground "#8aadf4" :background unspecified))))
    (face-spec-set 'helm-bookmark-directory '((t (:inherit helm-ff-directory))))
    (face-spec-set 'helm-bookmark-file '((t (:foreground "#cad3f5"))))
    (face-spec-set 'helm-ff-dotted-symlink-directory '((t (:foreground "#8bd5ca" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-swoop-target-line-face '((t (:background "#363a4f"))))
    (face-spec-set 'helm-swoop-target-line-block-face '((t (:foreground "#cad3f5" :background "#363a4f"))))
    (face-spec-set 'helm-swoop-target-word-face '((t (:background "#363a4f" :foreground "#a6da95"))))
    ;; Which-key: catppuccin only defines command-description and key-face
    (face-spec-set 'which-key-group-description-face '((t (:foreground "#8aadf4"))))
    (face-spec-set 'which-key-separator-face '((t (:background unspecified :foreground "#8bd5ca"))))
    (face-spec-set 'which-key-special-key-face '((t (:background "#c6a0f6" :foreground "#24273a"))))
    ;; Diff
    (face-spec-set 'diff-indicator-changed '((t (:background "#2a3548"))))
    ;; Swiper
    (face-spec-set 'swiper-line-face '((t (:background "#363a4f" :inherit bold))))
    (face-spec-set 'swiper-match-face-3 '((t (:foreground "#eed49f" :underline t))))
    (face-spec-set 'swiper-match-face-4 '((t (:foreground "#a6da95" :underline t))))
    ;; Dired: bold directories and symlinks like spacemacs-dark
    (face-spec-set 'dired-directory '((t (:foreground "#8aadf4" :inherit bold))))
    (face-spec-set 'dired-symlink '((t (:foreground "#f5bde6" :inherit bold))))
    (face-spec-set 'dired-marked '((t (:foreground "#c6a0f6" :inherit bold))))
    (face-spec-set 'dired-perm-write '((t (:foreground "#b8c0e0" :underline t))))
    ;; Completions: bold common part like spacemacs-dark
    (face-spec-set 'completions-common-part '((t (:foreground "#8aadf4" :weight bold))))
    ;; Helm: bold directories/symlinks and backgrounds like spacemacs-dark
    (face-spec-set 'helm-ff-directory '((t (:foreground "#8aadf4" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-ff-dotted-directory '((t (:foreground "#8aadf4" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-ff-symlink '((t (:foreground "#8bd5ca" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-ff-invalid-symlink '((t (:foreground "#ed8796" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-candidate-number '((t (:foreground "#eed49f" :background "#24273a" :inherit bold))))
    (face-spec-set 'helm-header '((t (:foreground "#b8c0e0" :background "#24273a" :underline nil :box nil))))
    (face-spec-set 'helm-separator '((t (:foreground "#c6a0f6" :background "#24273a"))))
    ;; Anzu: bold mode-line count like spacemacs-dark
    (face-spec-set 'anzu-mode-line '((t (:foreground "#eed49f" :inherit bold))))
    ;; Which-key: bold keys like spacemacs-dark
    (face-spec-set 'which-key-key-face '((t (:foreground "#ed8796" :inherit bold))))
    ;; Show-paren: bold + underline mismatch like spacemacs-dark
    (face-spec-set 'show-paren-mismatch '((t (:foreground "#ed8796" :inherit bold :underline t))))
    ;; Org: bold TODO/DONE with colored backgrounds like spacemacs-dark
    (face-spec-set 'org-todo '((t (:foreground "#f5a97f" :inherit bold :background "#32322c"))))
    (face-spec-set 'org-done '((t (:foreground "#a6da95" :inherit bold :background "#293235"))))
    (face-spec-set 'org-agenda-structure '((t (:foreground "#c6a0f6" :inherit bold))))
    ;; Markdown: bold headers like spacemacs-dark
    (face-spec-set 'markdown-header-face-1 '((t (:foreground "#ed8796" :inherit bold :height 1.3))))
    (face-spec-set 'markdown-header-face-2 '((t (:foreground "#f5a97f" :inherit bold :height 1.2))))
    ;; Tab-bar: bold active tab like spacemacs-dark
    (face-spec-set 'tab-bar-tab '((t (:foreground "#cad3f5" :background "#24273a" :weight bold))))
    ;; Rainbow-delimiters
    (face-spec-set 'rainbow-delimiters-mismatched-face '((t (:foreground "#ed8796" :overline t))))
    ;; Misc high-impact faces
    (face-spec-set 'link-visited '((t (:foreground "#c6a0f6" :underline t))))
    (face-spec-set 'page-break-lines '((t (:foreground "#494d64"))))
    (face-spec-set 'secondary-selection '((t (:background "#181926"))))
    (face-spec-set 'eldoc-highlight-function-argument '((t (:foreground "#a6da95" :inherit bold))))
    (face-spec-set 'hydra-face-blue '((t (:foreground "#8aadf4"))))
    (face-spec-set 'hydra-face-red '((t (:foreground "#ed8796"))))
    (face-spec-set 'highlight-symbol-face '((t (:background "#363a4f"))))
    (face-spec-set 'linum-relative-current-face '((t (:foreground "#c6a0f6"))))
    ;; System
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
