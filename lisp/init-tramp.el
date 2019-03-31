(setq tramp-verbose 2)
(setq tramp-default-method "ssh")
(setq tramp-use-ssh-controlmaster-options nil) ; Use the ssh controlmaster configuration

(setq tramp-default-user-alist
      '(
        ("\\`\\(?:fcp\\|krlogin\\|nc\\|r\\(?:cp\\|emcp\\|sh\\)\\|telnet\\|ssh\\)\\'" "user2" "root")
        ("\\`smb\\'" nil nil)
        ("\\`\\(?:fcp\\|krlogin\\|nc\\|r\\(?:cp\\|emcp\\|sh\\)\\|telnet\\|ssh\\)\\'" nil "endriu")
        ("\\`\\(?:ksu\\|su\\(?:do\\)?\\)\\'" nil "root")
        ("\\`\\(?:socks\\|tunnel\\)\\'" nil "endriu")
        ("\\`synce\\'" nil nil)
        )
      )

(provide 'init-tramp)
