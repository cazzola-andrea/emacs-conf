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
      (mapconcat 'identity res-list ":")))
  (message "No readable files in %s" root-path)
  )

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
  )


(use-package anaconda-mode
  :ensure t
  :config
  (setq anaconda-mode-lighter " 🐍")
  (define-key anaconda-mode-map (kbd "C-c C-d") 'anaconda-mode-show-doc)
  (define-key anaconda-mode-map (kbd "C-c .") 'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map (kbd "C-c ,") 'anaconda-mode-go-back)
  (define-key anaconda-mode-map (kbd "C-c C-r") 'anaconda-mode-find-references)
  (define-key anaconda-mode-map (kbd "C-c C-a") 'anaconda-mode-find-assignments)
  )

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(defun ac/py-list-classes-funcs ()
  "List all python classes and functions within a module."
  (interactive)
  (ac/toggle-match-highlight)
  (occur "^[ ]*\\(?:class\\|def\\) [^(^ ]+[ ]*(\\(?:[^:]*\\(?:\n[^:]*\\)*?\\)):")
  (ac/toggle-match-highlight)
  )

(define-key python-mode-map (kbd "C-C C-o") 'ac/py-list-classes-funcs)

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

(provide 'init-python)
;;; init-python ends here
