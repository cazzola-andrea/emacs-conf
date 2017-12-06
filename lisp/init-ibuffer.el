(use-package ibuffer
  :ensure t)

(use-package ibuffer-vc
  :ensure t)

(use-package ibuffer-tramp
  :ensure t
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (define-key ibuffer-mode-map (kbd "/ T")
    'ibuffer-tramp-set-filter-groups-by-tramp-connection)
  (setq ibuffer-sorting-mode "filename/process")
  )


(provide 'init-ibuffer)
