;;; init-python --- customized settings for python
;;; Commentary:


;;; Code:
(require 'init-coding)
(add-to-list 'char-highlighted-major-modes 'java-mode)


(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-prefer-flymake nil)
  :demand t
  :after jmi-init-platform-paths)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t)
  :after lsp-mode)

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package lsp-java
  :ensure t
  :init
  (defun jmi/java-mode-config ()
    (setq-local tab-width 4
                c-basic-offset 4)
    (toggle-truncate-lines 1)
    (setq-local tab-width 4)
    (setq-local c-basic-offset 4)
    (lsp))

  :config
  ;; Enable dap-java
  (require 'dap-java)

  ;; Support Lombok in our projects, among other things
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" jmi/lombok-jar)
              (concat "-Xbootclasspath/a:" jmi/lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build")

        lsp-java-import-order '["" "java" "javax" "#"]
        ;; Don't organize imports on save
        lsp-java-save-action-organize-imports nil

        ;; Formatter profile
        lsp-java-format-settings-url
        (concat "file://" jmi/java-format-settings-file))

  :hook (java-mode   . jmi/java-mode-config)

  :demand t
  :after (lsp lsp-mode dap-mode jmi-init-platform-paths))


(provide 'init-java)
;;; init-java ends here

