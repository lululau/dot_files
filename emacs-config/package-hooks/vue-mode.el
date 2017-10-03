(with-eval-after-load 'vue-mode
  (spacemacs/set-leader-keys-for-major-mode 'vue-mode "'" 'vue-mode-edit-indirect-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'vue-mode "r" 'vue-mode-reparse))
