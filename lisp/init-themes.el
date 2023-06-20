(package-install 'zenburn-theme)
(package-install 'leuven-theme)
(package-install 'hydandata-light-theme)
(package-install 'tron-legacy-theme)

;; load themes without enabling them and without asking for confirmation
(load-theme 'leuven t t)
(load-theme 'zenburn t t)
(load-theme 'misterioso t t)
(load-theme 'tron-legacy t t)
(load-theme 'acazzola-light t t)

(defvar current-dark-theme 'misterioso)
(defvar current-light-theme 'acazzola-light)

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

(light-theme)

(set-default 'cursor-type 'bar)
(set-default 'cursor-in-non-selected-windows nil)

;; set default font
(set-frame-font "SourceCodePro" t nil)
(set-face-attribute 'default nil :height 105)

;; force horizontal split
(setq split-height-threshold nil)
(setq split-width-threshold 160)


;; Configure powerline
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )


(provide 'init-themes)
;;; init-themes ends here
