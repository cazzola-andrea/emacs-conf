(server-start)

(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         )
      )

(package-initialize)

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
;; (require 'init-protobuf)
(require 'init-rest)
(require 'init-groovy)
(require 'init-fun)
(require 'init-tramp)

;; fonts
;; (use-package unicode-fonts
;;   :ensure t)
;; (unicode-fonts-setup)


(use-package ace-window
  :ensure t
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(background-color "#202020")
 '(background-mode dark)
 '(cursor-color "#cccccc")
 '(cursor-in-non-selected-windows nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "cdfc5c44f19211cfff5994221078d7d5549eeb9feda4f595a2fd8ca40467776c" "71c379d39642d7281407e56123ad7043b9874a1c18b20b6685730a86251a002e" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "ba7917b02812fee8da4827fdf7867d3f6f282694f679b5d73f9965f45590843a" "8c8b927e36470a3bc2b0182d8d19b815f5701cc0f306a4ccdc6a9a8a62a4bd6f" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "17cda1304ba8d26d62bf247cab2c161d12957054b6be4477abb5972a74eea4e1" "d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" "2917605f152dd02918c7fa59cfcc6ffd0c87c19615437851221966579925b242" "62a96c296a9dee45acc41cf80fe0b772a87723593c137cb2add3bc0fd1b3c120" "55f32648fbf91830c15ec2e44176e6d7afbe6b348fd2d951d61f78dc6c900837" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "8062d7fd259d3232d69b38db3b15d4ac44a70bf620cbc5b3926a6e16c74d6a5a" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "807a7f4c2d0d331fc1798e6d38b890ce3582096b8d622ba3b491b2aa4345e962" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "c36614262f32c16cd71e0561a26e5c02486b6a476a6adec7a5cc5582128e665e" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "50f92f4e3c507592538ec9fd01d030af0ad54f28353e4554ee47c521597f6ca2" "cb796d8e61a04f7ecefee7424ae5ce3b2771a9e169f2258abef4b0e3c3095585" "a066f01c54d7e1f4a1e7cbea6de5796e32cea68b89c184944b8bca5806cbd145" "47e349206916cde8b0b0c8b333592e8c9c9a7197442908b6d0be2117b8f093f2" "ad81985c992fc220f3b51057d0e9a77b0f7ec5d81481749cf8865493b607acbd" "c41b0e60500ece2dc3391294c822b8e5d6bf0eafd53ce452b38876087558cf13" "9037adbfe4bc9bc7ccef6d3f0de7525cfdb0808a2678f6d0db1c8038d74505c8" "7cba9408f31b2d5fdb55e4d81f826d1b20375f418733967e62a7adc545d528c1" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "84242949349fc6ccf3e61b0d220a43d2e5cbbf6d4b586b51a2658731058e1608" "63d172fcfd4f673201cfcb667f1f116cc7911a2a263daacc0f6beefb8bd67123" "714f6bf9b804c451e0524c31044d6affd28f2021bec5467f3daeaa6be8b0ec76" "eef52ccb4320eb2f0da05c1ac78ee3aae06ce9493624358e4850ffcdfb42257d" default)))
 '(fci-rule-color "#383838")
 '(flycheck-disabled-checkers (quote (puppet-lint)))
 '(foreground-color "#cccccc")
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(hl-sexp-background-color "#efebe9")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-src-block-faces (quote (("emacs-lisp" (:background "#F0FFF0")))))
 '(package-selected-packages
   (quote
    (ox-confluence org-contrib flx-ido hydandata-light-theme flymake-puppet csv-mode hacker-typer noctilux-theme organic-green-theme paganini-theme moe-theme browse-kill-ring groovy-mode protobuf-mode flycheck-protobuf zenburn-theme yaml-mode which-key use-package unicode-fonts test-simple restclient rainbow-delimiters puppet-mode pos-tip php-extras paredit multiple-cursors meacupla-theme logview loc-changes load-relative leuven-theme ido-vertical-mode ibuffer-vc ibuffer-tramp htmlize highlight-chars git-gutter+ ggtags focus flycheck fill-column-indicator ereader elpy dired-narrow company-jedi color-identifiers-mode camcorder ag ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(safe-local-variable-values (quote ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)"))))
 '(show-paren-mode t)
 '(split-height-threshold 100)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "gray10"))) t)
 '(my-tab-face ((((class color)) (:background "grey10"))) t)
 '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

(put 'narrow-to-region 'disabled nil)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
