(server-start)

(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         )
      )
(package-initialize)

;; If not installed, install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; ;; Force load of .bashrc when executing commands
;; (setq shell-command-switch "-ic")

;; Load variables from shell if in a mac
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

;; I don't need undo info on shell output
(defun disable-undo-shell ()
  (let ((name-buffer (buffer-name)))
    (if (equal "*Async Shell Command*" name-buffer)
        (buffer-disable-undo name-buffer)
      )
    )
  )
(add-hook 'comint-mode-hook 'disable-undo-shell)

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
(require 'init-cplus)
(require 'init-java)
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
(require 'init-pcap)
(require 'init-jsonnet)
(require 'init-rust)

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
(defvar ac/current-layout 'querty)
(defconst ac/aw-keys-colemack '(?a ?r ?s ?t ?n ?e ?i ?o))
(defconst ac/aw-keys-querty '(?a ?s ?d ?f ?j ?k ?l ?o))
(setq aw-keys ac/aw-keys-querty)

(defun ac/switch-avy-layout ()
  "Switch between layouts for avy-window-switch"
  (interactive)
  (if (eq ac/current-layout 'colemack)
      ;; switch to querty
      (progn (setq ac/current-layout 'querty)
             (setq aw-keys ac/aw-keys-querty))
    (progn (setq ac/current-layout 'colemack)
           (setq aw-keys ac/aw-keys-colemack)))
  )

(use-package browse-kill-ring
  :ensure t
  :init
  (setq browse-kill-ring-highlight-current-entry t)
  (global-set-key (kbd "C-x Y") 'browse-kill-ring)
  )


;; Need to set ASKPASS to work from command
(setenv "SUDO_ASKPASS" "ssh-askpass")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(async-shell-command-display-buffer nil)
 '(cursor-in-non-selected-windows nil)
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("e80b1078c4ce2225f6f8d7621a55d3b675c86cad505b22b20243d4d075f491f5" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "015ed1c4e94502568b7c671ced6fe132bec9edf72fd732aa59780cfbe4b7927c" "f3455b91943e9664af7998cc2c458cfc17e674b6443891f519266e5b3c51799d" "16658e18bd42cd2ae595110d3d8758eb30d7aaa38b84430a3169e54b30b244c3" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "6096a2f93610f29bf0f6fe34307587edd21edec95073cbfcfb9d7a3b9206b399" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" "05a4b82c39107308b5c3720fd0c9792c2076e1ff3ebb6670c6f1c98d44227689" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "3bc187cd480ad79f151b593f7cb7d4ad869b19741247589238c353f637e7fb21" "36bab4e2aa8165f538e6d223ee1d2a0ef918ccba09e18c62cf8594467685a3b6" "d70c11f5a2b69a77f9d56eff42090138721d4c51d9d39ce986680786d694f492" "5522d253b8958993ca744973fd013a0b4ab6b2f821aaafcf678ca27331dfabb2" "668793e975138624afffc010983862439fa01154ee96cf8da232cca81cb55d1e" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" zenburn leuven default))
 '(flycheck-checkers
   '(ada-gnat asciidoctor asciidoc awk-gawk bazel-build-buildifier bazel-module-buildifier bazel-starlark-buildifier bazel-workspace-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser python-flake8 python-pylint python-pycompile python-pyright python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint))
 '(flyspell-use-meta-tab nil)
 '(gnus-inhibit-images t)
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   '(file-info chocolate-theme jsonnet-mode git-modes coterm elf-mode ll-debug misterioso-theme misterioso marginalia pcap-mode libgit projectile-speedbar company-c-headers sr-speedbar lsp-ui tron-legacy-theme markdown-mode diff-at-point lush-theme x509-mode all docker-compose-mode docker-tramp dockerfile-mode hydandata-light-theme lab-themes flucui-themes visual-regexp-steroids visual-regexp f anaconda-eldoc-mode powerline zenburn-theme yasnippet-snippets yaml-mode which-key vlf use-package unicode-fonts test-simple restclient rainbow-delimiters puppet-mode protobuf-mode projectile pos-tip php-extras password-generator paredit multiple-cursors magit-popup logview loc-changes load-relative leuven-theme ido-vertical-mode ibuffer-vc ibuffer-tramp htmlize highlight-chars hacker-typer groovy-mode git-gutter+ ggtags focus flymake-puppet flycheck flx-ido fill-column-indicator ereader elpy dired-narrow diminish csv-mode company-jedi color-identifiers-mode camcorder browse-kill-ring ag ace-window))
 '(safe-local-variable-values
   '((auto-save-mode . -1)
     (auto-save-mode)
     (projectile-project-test-cmd . "ll_docker -D bionic run 'nosetests && ./pylint.sh'")
     (projectile-project-compilation-cmd . "dockerbuilder --tag 'dev-latest.bionic' build")))
 '(send-mail-function 'mailclient-send-it)
 '(shell-input-autoexpand 'input))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))

(put 'narrow-to-region 'disabled nil)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Start maximised
(setq frame-resize-pixelwise t)
(toggle-frame-maximized)

