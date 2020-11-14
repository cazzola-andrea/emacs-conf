;;; init-python --- customized settings for python
;;; Commentary:


;;; Code:
(require 'init-coding)
(add-to-list 'char-highlighted-major-modes 'java-mode)


(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'java-mode)
              (ggtags-mode 1)
              (subword-mode))))

;; (use-package dap-mode
;;   :diminish
;;   :bind
;;   (:map dap-mode-map
;;         (("<f12>" . dap-debug)
;;          ("<f8>" . dap-continue)
;;          ("<f9>" . dap-next)
;;          ("<M-f11>" . dap-step-in)
;;          ("C-M-<f11>" . dap-step-out)
;;          ("<f7>" . dap-breakpoint-toggle))))


;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;   (lsp-ui-doc-background ((t (:background nil))))
;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind
;;   (:map lsp-ui-mode-map
;;         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;         ([remap xref-find-references] . lsp-ui-peek-find-references)
;;         ("C-c u" . lsp-ui-imenu)
;;         ("M-i" . lsp-ui-doc-focus-frame))
;;   (:map lsp-mode-map
;;         ("M-n" . forward-paragraph)
;;         ("M-p" . backward-paragraph))
;;   :custom
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (if (display-graphic-p)
;;       (setq lsp-ui-doc-use-webkit t))
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

;; (use-package lsp-mode
;;   :defer t
;;   :commands lsp lsp-deferred
;;   :custom
;;   (lsp-auto-guess-root nil)
;;   (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   (lsp-file-watch-threshold 2000)
;;   (read-process-output-max (* 1024 1024))
;;   (lsp-eldoc-hook nil)
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;;   :hook ((java-mode) . lsp-deferred))

;; (use-package lsp-java
;;   :after lsp-mode
;;   :if (executable-find "mvn")
;;   :init
;;   (use-package request :defer t)
;;   :custom
;;   (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
;;   (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))

;; ;; Enable lsp-java-boot
;; (require 'lsp-java-boot)

;; ;; enable lenses
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

(provide 'init-java)
;;; init-java ends here

