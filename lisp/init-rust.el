;;; init-rust --- customized settings for python
;;; Commentary:


;;; Code:
(require 'init-coding)


(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . (lambda () (prettify-symbols-mode)))
  (rust-mode . eglot-ensure)
)


(provide 'init-rust)
;; init-rust ends here
