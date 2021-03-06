(use-package dash
  :ensure t)

(use-package with-editor
  :ensure t)

(use-package magit-popup
  :ensure t)

;; Required for magit
(use-package libgit
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/magit/lisp/")
(require 'magit)
  
(provide 'init-magit)
