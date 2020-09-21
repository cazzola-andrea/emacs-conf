(setq dired-listing-switches "-alh")

(use-package dired-narrow
  :ensure t
)

(eval-after-load 'dired-mode
  (define-key dired-mode-map
    (kbd "/") 'dired-narrow))

(provide 'init-dired)
