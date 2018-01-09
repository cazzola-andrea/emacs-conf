;; init-misc --- misc general purpose functions

;;; Commentary:

;;; Code:
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Ediff should split vertically
(eval-after-load 'ediff-mode
  (setq ediff-merge-split-window-function 'split-window-vertically)
  )

(eval-after-load 'sql
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq truncate-lines t)
              )
            )
  )

;; work with csv data
(use-package csv-mode
  :ensure t
  )

;; password creator
(use-package password-generator
  :ensure t
  )

;; work chat on emacs
;; (use-package slack
;;   :ensure t
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "emacs-slack"
;;    :default t
;;    :client-id "aaaaaaaaaaa.00000000000"
;;    :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
;;    :token "aaaa-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
;;    :subscribed-channels '(test-rename rrrrr))

;;   (slack-register-team
;;    :name "test"
;;    :client-id "3333333333.77777777777"
;;    :client-secret "cccccccccccccccccccccccccccccccc"
;;    :token "xxxx-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
;;    :subscribed-channels '(hoge fuga))
(provide 'init-misc)
;;; init-misc ends here
