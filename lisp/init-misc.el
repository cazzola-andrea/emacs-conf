;; init-misc --- misc general purpose functions

;;; Commentary:

;;; Code:
;;narrow dired to match filter
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; Ediff should split horizontally
(eval-after-load 'ediff-mode
  (setq ediff-split-window-function 'split-window-horizontally)
  )

(eval-after-load 'sql
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq truncate-lines t)
              )
            )
  )

;; Recognise MariaDB prompt
(eval-after-load 'sql
  (sql-set-product-feature 'mysql :prompt-regexp
                           "^\\(MariaDB\\|MySQL\\) \\[[\\(_a-zA-Z\\)]*\\]> ")
  )


;; work with csv data
(use-package csv-mode
  :ensure t
  )

;; password creator
(use-package password-generator
  :ensure t
  )

;; open huge files
(use-package vlf
  :ensure t
  :init (require 'vlf-setup)
  )

;; Configure powerline
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  )

(defvar ac/default-windows-configuration-register 48)

(defvar ac/window-configuration-register nil)

(defun ac/store-windows-configuration (&optional target-register)
  "Store current windows configuration.
if TARGET-REGISTER is specified, store configuration onto this
register, otherwise use DEFAULT-WINDOWS-CONFIGURATION-REGISTER."
  (interactive "P")
  (let ((window-target-register
         (if current-prefix-arg
           (register-read-with-preview
	    "Window configuration to register: ")
           ac/default-windows-configuration-register)))
    (window-configuration-to-register window-target-register)
    (setq ac/window-configuration-register window-target-register)
    ))

(defvar ac/window-maximised-p nil)

(defun ac/toggle-windows-maximisation (&optional arg)
  "Toggle maximisation on a window."
  (interactive "P")
  (if ac/window-maximised-p
      (progn
        (jump-to-register ac/window-configuration-register)
        (setq ac/window-maximised-p nil))
    (progn
      (ac/store-windows-configuration arg)
      (setq ac/window-maximised-p t)
      (delete-other-windows))
    )
  )

(global-set-key (kbd "C-x C-1") 'ac/toggle-windows-maximisation)

(defun ac/ansi-color-buffer ()
  "Render ansi colours in a given buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max))
  )

(defun ac/timestamp-to-date (timestamp)
  "Convert a UNIX TIMESTAMP into a readable datetime in UTC."
  (interactive "nTimestamp: ")
  (message (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time timestamp) t)))

(use-package x509-mode
  :ensure t)

;; Markdown mode
(use-package markdown-mode
  :ensure t)

;; Enable documentation in command in minibuffer completions
(use-package marginalia
  :ensure t
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(provide 'init-misc)
;;; init-misc ends here
