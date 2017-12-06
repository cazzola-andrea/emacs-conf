
(use-package logview
  :ensure t
  :config
  (add-to-list 'logview-additional-level-mappings
               '("Nginx-levels"
                 (error "error")
                 (warning)
                 (information)
                 (debug)
                 (trace)
                 (aliases)))
  (add-to-list 'logview-additional-level-mappings
               '("LL-levels"
                 (error "ERROR")
                 (warning "WARNING")
                 (information "INFO")
                 (debug "DEBUG")
                 (trace)
                 (aliases)))
  (add-to-list 'logview-additional-submodes
               '("LL-manager"
                  (format . "TIMESTAMP - NAME - LEVEL ")
                  (levels . "LL-levels")
                  (timestamp)
                  (aliases)))
  ;; (add-to-list 'logview-additional-submodes
  ;;              '("Nginx"
  ;;                (format . "TIMESTAMP ")
  ;;                (levels "Log4j")
  ;;                (timestamp . "YYYY/MM/DD HH:mm:ss")
  ;;                (aliases)))
  ;; (add-to-list 'logview-additional-timestamp-formats
  ;;              '("YYYY/MM/DD HH:mm:ss"
  ;;                (regexp . "[0-9]{4}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")))
  )



(provide 'init-log)
