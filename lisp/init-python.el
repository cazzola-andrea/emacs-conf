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
  (setenv "PYTHONPATH" (ac/compute-pythonpath "/home/endriu/work/utils"))

  :config
  (setq python-indent-offset 4)
  (setq fill-column 99)
  ;; (add-hook 'python-mode-hook 'color-identifiers-mode)
  ;; (add-hook 'python-mode-hook 'linum-mode)
  ;; (add-hook 'python-mode-hook 'ac/noob-mode)
  ;; (add-hook 'python-mode-hook 'ext/show-long-lines)
  ;; (add-hook 'python-mode-hook 'highlight-changes-mode)
  (setq python-check-command "flake8")
  (setq flycheck-flake8-maximum-line-length 99)
  ;; (add-hook 'python-mode-hook 'flycheck-mode)
  ;; (add-hook 'python-mode-hook 'flyspell-prog-mode)
  ;; (setq fci-rule-column 80)
  ;; (add-hook 'python-mode-hook 'fci-mode)
  (autoload 'jedi:setup "jedi" nil t)
  (add-hook 'python-mode-hook 'jedi:setup)
  )

(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python
          (elpy-enable))

  :config
  (electric-indent-local-mode -1)
  (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-check-command "epylint")
  (setq elpy-test-runner 'elpy-test-nose-runner)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-flymake elpy-modules)
  (delete 'elpy-django elpy-modules)
  (delete 'elpy-module-pyvenv elpy-modules)
  (setq elpy-rpc-error-timeout 300)
  (elpy-use-ipython)
  ;; (advice-add 'company-call-frontends :before #'on-off-fci-before-company)
  ;; (company-quickhelp-mode nil)
  (setq jedi:complete-on-dot t)
  (add-to-list 'elpy-project-ignored-directories "*.pyc")
  


  (defun ha/elpy-goto-definition ()
    (interactive)
    (condition-case err
      (elpy-goto-definition)
      ('error (find-tag (symbol-name (symbol-at-point))))))

  ;; :bind (:map elpy-mode-map ([remap elpy-goto-definition] . ha/elpy-goto-definition))
  )


(with-eval-after-load 'python
  (advice-add 'elpy-goto-definition :after-until 'find-tag))

(provide 'init-python)
;;; init-python ends here
