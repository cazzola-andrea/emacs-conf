(package-install 'zenburn-theme)
(package-install 'leuven-theme)

;; load themes without enabling them and without asking for confirmation
(load-theme 'leuven t t)
(load-theme 'zenburn t t)

(defvar current-dark-theme 'zenburn)
(defvar current-light-theme 'leuven)

(defun dark-theme ()
  "Enable dark theme of choice."
  (interactive)
  (message "Going dark")
  (disable-theme current-light-theme)
  (enable-theme current-dark-theme)
  )

(defun light-theme ()
  "Enable light-theme of choice."
  (interactive)
  (message "There be light")
  (disable-theme current-dark-theme)
  (enable-theme current-light-theme)
  )

(dark-theme)

(provide 'init-themes)
;;; init-themes ends here
