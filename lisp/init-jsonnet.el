;;; init-jsonnet -- customised settings for jsonnet
;;; Commentary:


;;; Code:
(require 'init-coding)

(add-to-list 'char-highlighted-major-modes 'python-mode)

(use-package jsonnet-mode
  :ensure t
  :mode ("\\.jsonnet$" . jsonnet-mode)
        ("\\.libsonnet$" . jsonnet-mode)
  )

(provide 'init-jsonnet)
;;; init-jsonnet ends here
