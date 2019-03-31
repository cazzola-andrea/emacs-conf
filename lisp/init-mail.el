;;; init-mail --- Email client configuration
;;; Commentary:


;;; Code:

(use-package smtpmail)
(use-package gnus
  :config
  (setq gnus-parameters'((".*" (display . all))))
  (setq gnus-thread-indent-level 1)
  (define-key gnus-group-mode-map "q" 'bury-buffer)
  )

(setq user-mail-address "acazzola@lastline.com"
      user-full-name "Andrea Cazzola")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(provide 'init-mail)
;;; init-mail.el ends here
