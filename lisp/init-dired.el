(setq dired-listing-switches "-alh")

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))


(provide 'init-dired)
