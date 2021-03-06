;;; init-mail --- Email client configuration
;;; Commentary:


;;; Code:

(use-package smtpmail)
(use-package gnus
  :config
  (setq gnus-parameters'((".*" (display . all))))
  (setq gnus-group-sort-function 'gnus-group-sort-by-alphabet)
  (setq gnus-thread-indent-level 1)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-number)
          (not gnus-thread-sort-by-date)))
  )

(setq user-mail-address "acazzola@lastline.com"
      user-full-name "Andrea Cazzola")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it)

(defun ac/set-smtp-gmail ()
  "Set up mail client to send mail w Gmail."
  (interactive)
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
        )
)

(defun ac/set-smtp-sensor ()
  "Set up smtp client to send to LL sensor."
  (interactive)
  (setq smtpmail-smtp-server "sensor-cloud"
        smtpmail-smtp-service 25)
  )

(ac/set-smtp-gmail)
  
(provide 'init-mail)
;;; init-mail.el ends here
