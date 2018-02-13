;; init-misc --- misc general purpose functions

;;; Commentary:

;;; Code:
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; force horizontal split
(setq split-height-threshold nil)

;; Ediff should split horizontally
(eval-after-load 'ediff-mode
  (setq ediff-split-window-function 'split-window-horizontally)
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

;; open huge files
(use-package vlf
  :ensure t
  :init (require 'vlf-setup)
  )

;; set default font
(set-frame-font "SourceCodePro")

(provide 'init-misc)
;;; init-misc ends here
