;;; init-python --- customized settings for python
;;; Commentary:


;;; Code:
(require 'init-coding)

(add-to-list 'char-highlighted-major-modes 'python-mode)

;; To deal with unknown codebases I prefer
;; to start as a noob
(defun ac/noob-mode ()
  (read-only-mode t))

(defun ac/compute-pythonpath (root-path)
  (if (file-accessible-directory-p root-path)
      (let (
            (res-list '())
            (content-list (directory-files root-path t))
            (temp-python-path (getenv "PYTHONPATH"))
            )
        (while content-list
          (let ((cur-file (car content-list)))
            (cond
             ((and
               (file-directory-p cur-file)
               (file-readable-p cur-file)
               (not (string-match-p "\\." cur-file)))
              (setq res-list (cons cur-file res-list))))
            )
        (setq content-list (cdr content-list)))
        (mapconcat 'identity res-list ":"))
    (message "No readable files in '%s'" root-path))
  )

(setenv "PYTHONPATH" (ac/compute-pythonpath "/home/endriu/work/code"))

;; copy some of elpy functions to move around code
(defun elpy--nav-move-region-vertically (beg end dir)
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))

(defun elpy--nav-move-line-vertically (dir)
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun elpy-nav-move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end 1)
    (elpy--nav-move-line-vertically 1)))

(defun elpy-nav-move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (elpy--nav-move-region-vertically beg end -1)
    (elpy--nav-move-line-vertically -1)))

(defun elpy-nav-forward-block ()
  "Move to the next line indented like point.
This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (when (equal (point) cur)
        (error "Statement does not finish"))
      (setq cur (point))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-backward-block ()
  "Move to the previous line indented like point.
This will skip over lines and statements with different
indentation levels."
  (interactive "^")
  (let ((indent (current-column))
        (start (point))
        (cur nil))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-backward-statement)
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (when (equal (point) cur)
        (error "Statement does not start"))
      (setq cur (point))
      (python-nav-backward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-forward-indent ()
  "Move forward to the next indent level, or over the next word."
  (interactive "^")
  (if (< (current-column) (current-indentation))
      (let* ((current (current-column))
             (next (* (1+ (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (let ((eol (point-at-eol)))
      (forward-word)
      (when (> (point) eol)
        (goto-char (point-at-bol))))))

(defun elpy-nav-backward-indent ()
  "Move backward to the previous indent level, or over the previous word."
  (interactive "^")
  (if (and (<= (current-column) (current-indentation))
           (/= (current-column) 0))
      (let* ((current (current-column))
             (next (* (1- (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (backward-word)))

;; end of elpy functions


;; (use-package f
;;   :ensure t
;;   )

;; (use-package pythonic
;;   :load-path "~/.emacs.d/site-lisp/pythonic"
;;   :config
;;   )

(defun ac/py-list-classes-dispatch (generic-argument)
  "Dispatch correct occur in python files."
  (interactive "P")
    (if (eq nil generic-argument)
      (ac/py-list-classes-funcs)
    (let ((current-prefix-arg nil))
      (ac/py-list-classes))
    ))

(defconst ac/multiline-python-header-re
  "^[ ]*%s \\(?:.*?\\(\n.*?\\)*?\\):$")

(defun ac/py-list-classes-funcs ()
  "List all python classes and functions within a module."
  (interactive)
  (let ((class-def-search-re "\\(?:class\\|def\\)"))
    (ac/toggle-match-highlight)
    (occur (format ac/multiline-python-header-re class-def-search-re))
    (ac/toggle-match-highlight)
    )
  )

(defun ac/py-list-classes ()
  "List all python classes within a module."
  (interactive)
  (let ((class-search-re "class"))
    (ac/toggle-match-highlight)
    (occur (format ac/multiline-python-header-re class-search-re))
    (ac/toggle-match-highlight)
    )
  )


;; ;; python style regexps
;; (use-package visual-regexp
;;   :ensure t)

;; (use-package visual-regexp-steroids
;;   :ensure t
;;   :config
;;   (define-key global-map (kbd "M-s r") 'vr/mc-mark))

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4)
  (setq fill-column 99)
  (setq python-shell-interpreter "ipython3")
  (define-key python-mode-map (kbd "<M-right>") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "<M-left>") 'python-indent-shift-left)
  (define-key python-mode-map (kbd "<M-up>") 'elpy-nav-move-line-or-region-up)
  (define-key python-mode-map (kbd "<M-down>") 'elpy-nav-move-line-or-region-down)
  (define-key python-mode-map (kbd "<C-up>") 'elpy-nav-backward-block)
  (define-key python-mode-map (kbd "M-p") 'elpy-nav-backward-block)
  (define-key python-mode-map (kbd "<C-down>") 'elpy-nav-forward-block)
  (define-key python-mode-map (kbd "M-n") 'elpy-nav-forward-block)
  (define-key python-mode-map (kbd "<C-right>") 'elpy-nav-forward-indent)
  (define-key python-mode-map (kbd "<C-left>") 'elpy-nav-backward-indent)
  (define-key python-mode-map (kbd "C-C C-o") 'ac/py-list-classes-dispatch)
  (define-key python-mode-map (kbd "C-c C-d") 'eldoc)
  (define-key python-mode-map (kbd "C-c .") 'xref-find-definitions)
  (define-key python-mode-map (kbd "C-c C-r") 'xref-find-references)
  (define-key python-mode-map (kbd "C-c ,") 'xref-go-back)
  (define-key python-mode-map (kbd "C-c c") 'company-complete)
  (define-key python-mode-map (kbd "C-c R") 'eglot-rename))

(add-hook 'python-mode-hook 'eglot-ensure)
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:mypy (:live_mode :json-false)))))
;; Disable pylint by default (should work faster)
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:pylint (:enabled :json-false)))))
(setq-default eglot-workspace-configuration
              '(:pylsp (:plugins (:pycodestyle (:maxLineLength 99)))))



(provide 'init-python)
;;; init-python ends here
