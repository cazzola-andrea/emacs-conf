(require 'init-coding)

(defconst ac/protobuf-style
  '((c-basic-offset . 4)
    (indent-tabs-mode . nil))
  )

(use-package protobuf-mode
  :ensure t
  :init
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "ac/protobuf-style" ac/protobuf-style t)))
  (add-to-list 'char-highlighted-major-modes 'protobuf-mode)
)

(provide 'init-protobuf)
