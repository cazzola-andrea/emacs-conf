(server-start)

(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         )
      )

(package-initialize)

;; ;; Force load of .bashrc when executing commands
;; (setq shell-command-switch "-ic")

;; I don't need undo info on shell output
(defun disable-undo-shell ()
  (let ((name-buffer (buffer-name)))
    (if (equal "*Async Shell Command*" name-buffer)
        (buffer-disable-undo name-buffer)
      )
    )
  )
(add-hook 'comint-mode-hook 'disable-undo-shell)

;; Install use-package from elpa
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)

;; Load from git repo
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
  (require 'use-package))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(setq initial-scratch-message "") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps

(when (window-system)
  (tool-bar-mode 0)               ;; Toolbars were only cool with XEmacs
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1)
  (menu-bar-mode -1))            ;; Scrollbars are waste screen estate

;; enable auto-revert over tramp
;; since I'm always working via tramp
(setq auto-revert-remote-files t)


(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-themes)
(require 'init-misc)
(require 'init-dired)
(require 'init-ido)
(require 'init-coding)
(require 'init-python)
(require 'init-magit)
(require 'init-org)
(require 'init-yaml)
(require 'init-ibuffer)
(require 'init-log)
(require 'init-puppet)
(require 'init-json)
(require 'init-protobuf)
(require 'init-rest)
(require 'init-groovy)
(require 'init-fun)
(require 'init-tramp)
(require 'init-mail)

;; fonts
;; (use-package unicode-fonts
;;   :ensure t)
;; (unicode-fonts-setup)


(use-package ace-window
  :ensure t
  :init
    (global-set-key (kbd "C-x o") 'ace-window)
    :diminish ace-window-mode)

;; switch between layouts for avy
(defvar ac/current-layout 'colemack)
(setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

(defun ac/swith-avy-layout ()
  "Switch between layouts for avy-window-switch"
  (interactive)
  (if (eq ac/current-layout 'colemack)
      ;; switch to querty
      (progn (setq ac/current-layout 'querty)
             (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o)))
    (progn (setq ac/current-layout 'colemack)
           (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))))
  )

(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-highlight-current-entry t)
  (global-set-key (kbd "C-x Y") 'browse-kill-ring)
  )


;; Need to set ASKPASS to work from command
(setenv "SUDO_ASKPASS" "/usr/bin/ssh-askpass")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-display-buffer nil)
 '(custom-safe-themes
   (quote
    ("05a4b82c39107308b5c3720fd0c9792c2076e1ff3ebb6670c6f1c98d44227689" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "3bc187cd480ad79f151b593f7cb7d4ad869b19741247589238c353f637e7fb21" "36bab4e2aa8165f538e6d223ee1d2a0ef918ccba09e18c62cf8594467685a3b6" "d70c11f5a2b69a77f9d56eff42090138721d4c51d9d39ce986680786d694f492" "5522d253b8958993ca744973fd013a0b4ab6b2f821aaafcf678ca27331dfabb2" "668793e975138624afffc010983862439fa01154ee96cf8da232cca81cb55d1e" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" zenburn leuven default)))
 '(flyspell-use-meta-tab nil)
 '(gnus-inhibit-images t)
 '(package-selected-packages
   (quote
    (x509-mode all docker-compose-mode docker-tramp dockerfile-mode hydandata-light-theme lab-themes flucui-themes visual-regexp-steroids visual-regexp f anaconda-eldoc-mode spaceline powerline zenburn-theme yasnippet-snippets yaml-mode which-key vlf use-package unicode-fonts test-simple restclient rainbow-delimiters puppet-mode protobuf-mode projectile pos-tip php-extras password-generator paredit multiple-cursors magit-popup logview loc-changes load-relative leuven-theme ido-vertical-mode ibuffer-vc ibuffer-tramp htmlize highlight-chars hacker-typer groovy-mode git-gutter+ ggtags focus flymake-puppet flycheck flx-ido fill-column-indicator ereader elpy dired-narrow diminish csv-mode company-jedi color-identifiers-mode camcorder browse-kill-ring ag ace-window)))
 '(send-mail-function (quote mailclient-send-it))
 '(shell-input-autoexpand (quote input)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'narrow-to-region 'disabled nil)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
