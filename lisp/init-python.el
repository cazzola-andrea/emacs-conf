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
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4)
  (setq fill-column 99)
  (setq flycheck-flake8-maximum-line-length 99)
  )


(use-package anaconda-mode
  :ensure t
  :config
  (setq anaconda-mode-lighter " 🐍")
  (define-key anaconda-mode-map (kbd "C-c .") 'anaconda-mode-show-doc)
  )

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(defun ac/py-list-classes-funcs ()
  "List all python classes and functions within a module."
  (interactive)
  (occur "^[ ]*\\(?:class\\|def\\) [^(]+(\\(?:[^:]*\\(?:\n[^:]*\\)*?\\)):")
  )

(define-key python-mode-map (kbd "C-C C-o") 'ac/py-list-classes-funcs)

(provide 'init-python)
;;; init-python ends here
