(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "C-s") 'company-complete-common-or-cycle)))

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "C-r") 'company-select-previous)))


;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'company-mode)
;;   (setq company-gtags-modes '())
;; )

(use-package company-anaconda
  :ensure t
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (add-hook 'prog-mode-hook 'company-mode)
  )

(provide 'init-company)


