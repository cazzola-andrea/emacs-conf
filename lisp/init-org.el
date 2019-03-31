(use-package htmlize
  :ensure t)


(add-to-list 'char-highlighted-major-modes 'org-mode)

(defun ac/open-org-notes ()
  "Open file with notes."
  (interactive)
  (find-file org-default-notes-file))

(use-package org
  :ensure t        ; But it comes with Emacs now!?
  :init
  (setq org-use-speed-commands t
        org-hide-emphasis-markers t
        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-default-notes-file "~/.notes.org"
        org-todo-keywords '((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)")
                            (sequence "|" "CANCELED(c)")))
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
  (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))   ;; Journal entries
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (visual-line-mode t)  ; I want the text to be seen inside the buffer
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-M-|" . indent-rigidly)
         ("C-c n" . ac/open-org-notes))
  :config
  (font-lock-add-keywords            ; A bit silly but my headers are now
   'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                          nil)))
               ("^\\*+ \\(DOING\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                          nil)))
               ("^\\*+ \\(CANCELED\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                          nil)))
               ("^\\*+ \\(DONE\\) "
                (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                          nil)))))

  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent)))))


(use-package org
  :init
  (font-lock-add-keywords 'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (js         . t)
     (emacs-lisp . t)
     (perl       . t)
     (python     . t)
     (ruby       . t)
     (dot        . t)
     (css        . t)
     (plantuml   . t))))

;; (eval-after-load 'org-src
;;   '(define-key org-src-mode-map
;;      (kbd "C-x C-s") #'org-edit-src-exit))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defadvice org-edit-src-code (around set-buffer-file-name activate compile)
  (let ((file-name (buffer-file-name))) ;; (1)
    ad-do-it                            ;; (2)
    (setq buffer-file-name file-name))) ;; (3)

(require 'epa-file)
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

;; add contrib modules (confluence)
(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/contrib/lisp" t)
(require 'ox-confluence)

(provide 'init-org)
