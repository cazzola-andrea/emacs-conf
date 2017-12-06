(require 'init-coding)

(use-package puppet-mode
  :ensure t
  :config
  (setq puppet-indent-level 4)
  (delete 'puppet-lint flycheck-checkers)
  )

(add-to-list 'char-highlighted-major-modes 'puppet-mode)

(provide 'init-puppet)
