;; init-misc --- misc general purpose functions

;;; Commentary:

;;; Code:
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Ediff should split vertically
(eval-after-load 'ediff-mode
  (setq ediff-merge-split-window-function 'split-window-vertically)
  )

(eval-after-load 'sql
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq truncate-lines t)
              )
            )
  )

;; work with csv data
(use-package csv-mode
  :ensure t
  )

;; password creator
(use-package password-generator
  :ensure t
  )

(provide 'init-misc)
;;; init-misc ends here
