;;; init-python --- customized settings for python
;;; Commentary:


;;; Code:
(require 'init-coding)
(add-to-list 'char-highlighted-major-modes 'java-mode)


(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'java-mode)
              (ggtags-mode 1)
              (subword-mode))))


(provide 'init-java)
;;; init-java ends here

