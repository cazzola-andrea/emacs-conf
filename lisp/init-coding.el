;;; package --- Summary

;;; Commentary:

;;; Code:

;; (ext/show-long-lines 80)
(require 'init-company)


(defvar char-highlighted-major-modes '()
  "List of modes where we highlight characters."
  )

(defun ac/highlight-trailing-whitespace ()
  "Control which modes get character highlighting."
  (if
      (and
       (not (null (member major-mode char-highlighted-major-modes)))
       (not (eq hc-highlight-trailing-whitespace-p t))
       )
      (hc-toggle-highlight-trailing-whitespace)
    (if
        (and
         (null (member major-mode char-highlighted-major-modes))
         (eq hc-highlight-trailing-whitespace-p t))
        (hc-toggle-highlight-trailing-whitespace)
      )
    )
  )
    

;; hc-toggle-highlight-whitespace
(use-package highlight-chars
  :ensure t
  :config
  (add-hook 'change-major-mode-hook
            'ac/highlight-trailing-whitespace
            'APPEND)
  (add-hook 'after-change-major-mode-hook
            'ac/highlight-trailing-whitespace
            'APPEND)
  )

(use-package diminish
  :ensure t
  )

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :init
  (setq which-key-idle-delay 1)
  (which-key-mode)
)

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :config
  (setq flycheck-idle-change-delay 3)
  (delete 'new-line flycheck-check-syntax-automatically)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package ggtags
  :ensure t)

(require 'init-magit)

(use-package projectile
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'projectile-mode)
  ;; (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-project-root-cache-predicate 'file-remote-p)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'magit-status)
  (setq projectile-global-mode nil)
  (setq projectile-mode-line '(:eval
                               (format " P[%s]"
                                       (projectile-project-name))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

(defun ac/discover-projecs (directory)
  "Fix to projectile discover projects"
  (interactive
   (list (read-directory-name "Starting directory: ")))
    (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (let ((default-directory dir)
               (projectile-cached-project-root nil))
           (when (projectile-project-p)
             (projectile-add-known-project (projectile-project-root))))))
     subdirs)))

;; Use side bar
(use-package sr-speedbar
  :ensure t)

(use-package projectile-speedbar
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p |") 'projectile-speedbar-toggle)
  )

(use-package flx-ido
  :ensure t)

(use-package color-identifiers-mode
  :diminish color-identifiers-mode
  :ensure t
  :commands color-identifiers-mode
  :config
  (add-hook 'prog-mode-hook 'color-identifiers-mode)
)

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fill-column-indicator t)
)

;; (defun on-off-fci-before-company(command)
;;   (when (string= "show" command)
;;     (turn-off-fci-mode))
;;   (when (string= "hide" command)
;;     (turn-on-fci-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
)

(use-package flyspell
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(use-package git-gutter+
  :diminish git-gutter+-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'git-gutter+-mode)
)


(defun ac/occur (generic-argument)
  "Customized version of occur.
If GENERIC-ARGUMENT is specified, it works as occur, while without
arguments performs an occur call on the symbol at point."
  (interactive "P")
  (if (eq nil generic-argument)
      (occur
       (thing-at-point 'symbol t))
    (let ((current-prefix-arg nil))
      (call-interactively 'occur))
    )
  )

;; I want to have this instead of simple occur
(global-set-key (kbd "M-s o") 'ac/occur)


;; Manage highligh (useful for full line occur)
(defvar previous-match-highlight nil)

(if list-matching-lines-face
    (setq previous-match-highlight list-matching-lines-face))

(defun ac/toggle-match-highlight ()
  "Toggle highlight face."
  (interactive)
  (if list-matching-lines-face
      (progn
        (setq previous-match-highlight list-matching-lines-face)
        (setq list-matching-lines-face nil)
        (message "Highlight matches is OFF"))
    (progn
      (setq list-matching-lines-face previous-match-highlight)
      (setq previous-match-highlight nil)
      (message "Highlight matches is ON")))
  )

(define-key occur-mode-map (kbd "t") 'ac/toggle-match-highlight)

;; aliases to coding systems
(define-coding-system-alias 'utf8 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'UTF8 'utf-8)


(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e" . mc/edit-lines))
  )

(use-package ag
  :ensure t)


;; yasnippet does not build snippets by default anymore.
;; I'll install it explicitly and download the snippets
(use-package yasnippet
  :diminish 'yas-minor-mode
  :ensure t
  :config
  (yas-load-directory "~/.emacs.d/snippets/"))

(use-package yasnippet-snippets
  :ensure t)

(add-hook 'prog-mode-hook 'yas-minor-mode)

;; highlight brackets
(progn
  (show-paren-mode 1)
  (setq show-paren-style 'parenthesis)
  )

;; RFC pages
(add-to-list 'load-path "~/.emacs.d/site-lisp/rfcview.el")
(require 'rfcview)

;; Embedded ruby mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eruby-mode")
(require 'eruby-mode)
(set-face-background 'eruby-standard-face "gainsboro")
(set-face-attribute 'eruby-standard-face nil :slant 'italic)

;; add infrastructure tools to path
(setenv "PATH" (concat (getenv "PATH") ":/home/endriu/work/code/infrastructure-tools/scripts"))

(provide 'init-coding)
;;; init-coding ends here
