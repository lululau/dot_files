(spacemacs|use-package-add-hook docker
  :post-config
  (progn
    (evilified-state-evilify docker-image-mode docker-image-mode-map)
    (evilified-state-evilify docker-container-mode docker-container-mode-map)
    (evilified-state-evilify docker-network-mode docker-network-mode-map)
    (evilified-state-evilify docker-volume-mode docker-volume-mode-map)))
