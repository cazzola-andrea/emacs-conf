;;; init-cplus --- Customised settings for C/C++
;;; Commentary:

;;; Code:
(require 'cc-mode)
(require 'init-coding)
(add-to-list 'char-highlighted-major-modes 'c-mode 'cc-mode)

(require 'ggtags)
(add-hook 'c-mode-common-hook 'ggtags-mode)

;; ggtags keybinds
(define-key ggtags-mode-map (kbd "C-c .") 'ggtags-find-tag-dwim)
(define-key ggtags-mode-map (kbd "C-c s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(define-key ggtags-mode-map (kbd "C-c ,") 'pop-tag-mark)

;; Code completion with company-clang
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;; Header completion
(use-package company-c-headers
  :ensure t)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/7.5.0/")

;; Add semantic support
;; (require 'cc-mode)
;; (require 'semantic)

;; (add-hook 'c-mode-common-hook semantic-mode)

;; (add-to-list 'company-backends 'company-semantic)

;; ggtags && eldoc
(define-minor-mode c-eldoc-minor-mode
  "Toggle echo area display of C objects at point"
  :lighter ""
  (if c-eldoc-minor-mode
      (turn-on-c-eldoc-minor-mode)
    (turn-off-c-eldoc-minor-mode)))

(defun turn-on-c-eldoc-minor-mode ()
    "Turn on `c-eldoc-minor-mode'."
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function 'ggtags-eldoc-function)
  (eldoc-mode +1))

(defun turn-off-c-eldoc-minor-mode ()
  "Turn off `c-eldoc-minor-mod'."
  (kill-local-variable 'eldoc-documentation-function)
  (eldoc-mode -1))

(add-hook 'c-mode-common-hook 'c-eldoc-minor-mode)

;; Code movements
(define-key c++-mode-map (kbd "M-p") 'beginning-of-defun)
(define-key c++-mode-map (kbd "M-n") 'end-of-defun)

;; List function names

(provide 'init-cplus)
;;; init-cplus ends here
