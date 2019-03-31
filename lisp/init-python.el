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


(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4)
  (setq fill-column 99)
  (setq flycheck-flake8-maximum-line-length 99)
  (setq python-shell-interpreter "ipython")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ll_docker")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "docker")
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
  )

(use-package f
  :ensure t
  )

(defvar pythonic-standard-directory "~/work/venv")

;; (defun ac/pythonic-auto-setup ()
;;   (let ((dir pythonic-standard-directory))
;;     (when (and (file-directory-p dir)
;;                (not pythonic-environment))
;;       (add-to-list 'python-shell-extra-pythonpaths "/usr/lib/python2.7/dist-packages")
;;       (pythonic-activate dir))
;;     ))

(use-package pythonic
  :load-path "~/.emacs.d/site-lisp/pythonic"
  :config
  ;; (add-hook 'python-mode-hook 'ac/pythonic-auto-setup)
  ;; (add-to-list 'python-shell-extra-pythonpaths "/usr/lib/python2.7/dist-packages")
  )



(use-package anaconda-mode
  :load-path "~/.emacs.d/site-lisp/anaconda-mode/"
  :diminish anaconda-mode
  :diminish eldoc-mode
  :config
  (setq anaconda-mode-lighter " 🐍")
  (define-key anaconda-mode-map (kbd "C-c C-d") 'anaconda-mode-show-doc)
  (define-key anaconda-mode-map (kbd "C-c .") 'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map (kbd "C-c ,") 'anaconda-mode-go-back)
  (define-key anaconda-mode-map (kbd "C-c C-r") 'anaconda-mode-find-references)
  (define-key anaconda-mode-map (kbd "C-c C-a") 'anaconda-mode-find-assignments)
  (define-key anaconda-mode-map (kbd "C-c c") 'anaconda-mode-complete)
  )

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(defun ac/py-list-classes-dispatch (generic-argument)
  "Dispatch correct occur in python files."
  (interactive "P")
    (if (eq nil generic-argument)
      (ac/py-list-classes-funcs)
    (let ((current-prefix-arg nil))
      (ac/py-list-classes))
    )
  )

(defun ac/py-list-classes-funcs ()
  "List all python classes and functions within a module."
  (interactive)
  (ac/toggle-match-highlight)
  (occur "^[ ]*\\(?:class\\|def\\) [^(^ ]+[ ]*(\\(?:[^:]*\\(?:\n[^:]*\\)*?\\)):")
  (ac/toggle-match-highlight)
  )

(defun ac/py-list-classes ()
  "List all python classes within a module."
  (interactive)
  (ac/toggle-match-highlight)
  (occur "^[ ]*class [^(^ ]+[ ]*(\\(?:[^:]*\\(?:\n[^:]*\\)*?\\)):")
  (ac/toggle-match-highlight)
  )

(define-key python-mode-map (kbd "C-C C-o") 'ac/py-list-classes-dispatch)

;; use inferior python mode for async command invoking a python shell
(defun ac/python-shell-mode (buffer-name)
  "Activate inferior python mode for shell commands, renaming buffer to BUFFER-NAME."
  (interactive "B")
  (setq python-shell--interpreter "ipython")
  (setq python-shell--interpreter-args "-i")
  (inferior-python-mode)
  (rename-buffer buffer-name)
  (buffer-enable-undo)
  )

;; python style regexps
(use-package visual-regexp
  :ensure t)

(use-package visual-regexp-steroids
  :ensure t
  :config
  (define-key global-map (kbd "M-s r") 'vr/mc-mark))

(eval-after-load 'projectile-mode
  (projectile-register-project-type 'python-lastline '("setup.py")
                                    :compile "lastline_make_deb -d"
                                    :test "nosetests"
                                    :test-prefix "test_"
                                    :test-suffix"_test"))


(provide 'init-python)
;;; init-python ends here
