;;; init.el --- Self-installing, for Emacs 24.4+

;;; Commentary:

;; My Emacs configuration for writing code and things.

;;; Code:
(defconst emacs-start-time (current-time))
(defconst my-home (getenv "HOME"))
(defconst my-bin (concat my-home "/bin"))
(defconst my-src (concat my-home "/src"))
;; user-emacs-directory is provided too
;; late for my purposes so I set this now.
(defconst dot-emacs (concat my-home "/.emacs.d"))

;; Some initial package stuff
(require 'package)
(setq
 ;; Keep custom stuff out of here!
 custom-file (concat dot-emacs "/my-custom.el")
 package-archives
 ;; GNU over SSL
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
   ("melpa" . "https://melpa.org/packages/")
   ;; MELPA Stable
   ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; TODO: This is necessary to trust sml themes, but can
;; the later, redundant loading of custom-file be stopped?
(load custom-file :noerror :nomessage)

;; Pin here because use-package doesn't sseem to be able to...
;; https://github.com/jwiegley/use-package/issues/343
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(ensime . "melpa-stable") t)

(package-initialize)

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use some packages (configure them too!)
;; https://github.com/jwiegley/use-package/
(defvar use-package-verbose t)
(eval-when-compile (require 'use-package))

(require 'bind-key)

(use-package autorevert :diminish auto-revert-mode)

;; Tool for capturing screencasts directly from Emacs.
;; https://github.com/Malabarba/camcorder.el
(use-package camcorder
  :defer t
  :ensure t
  :config
  (custom-set-variables
   '(camcorder-frame-parameters
     (quote
      ((height . 45)
       (width . 110))))
   '(camcorder-gif-output-directory "~/videos/emacs/gifs")
   '(camcorder-output-directory "~/videos/emacs")))

;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C,
;; Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :bind
  ("M-e" . nil)
  ("M-j" . nil)
  :defer t
  :config
  (defun clang-format-save-hook ()
    "Run clang-format on save when in c or c++ mode when the variable
     'clang-forma-on-save' is set. Put the following into a .dir-locals.el file
     in your project to use this:

     ((nil . ((eval . (setq clang-format-on-save t)))))"
    (interactive)
    (defvar clang-format-on-save)
    (when (and clang-format-on-save
               (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)))
      (clang-format-buffer)))
  (add-hook 'before-save-hook 'clang-format-save-hook)
  (defvar company-backends (delete 'company-semantic company-backends))
  (define-key c-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map [(tab)] 'company-complete))

;; Clojure -- to use or not to use?
(let ((use-clojure (getenv "EMACS_CLOJURE")))
  (if (not (equal use-clojure nil))
      (load "~/.emacs.d/extra/clojure")))

;; Clang format
;; http://clang.llvm.org/docs/ClangFormat.html
;; http://clang.llvm.org/docs/ClangFormatStyleOptions.html
(use-package clang-format :ensure t :defer t)

;; Clean auto-indent and backspace unindent
;; https://github.com/pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :ensure t)

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :diminish company-mode
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20)
  ;; Default colors are awful - borrowed these from gocode (thanks!):
  ;; https://github.com/nsf/gocode/tree/master/emacs-company#color-customization
  (set-face-attribute
   'company-preview nil :foreground "black" :underline t)
  (set-face-attribute
   'company-preview-common nil :inherit 'company-preview)
  (set-face-attribute
   'company-tooltip nil :background "lightgray" :foreground "black")
  (set-face-attribute
   'company-tooltip-selection nil :background "steelblue" :foreground "white")
  (set-face-attribute
   'company-tooltip-common nil :foreground "darkgreen" :weight 'bold)
  (set-face-attribute
   'company-tooltip-common-selection nil :foreground "black" :weight 'bold))

;; Ansible keywords completion for Emacs
;; https://github.com/krzysztof-magosa/company-ansible
(use-package company-ansible :defer t :ensure t)

;; Auto-completion for C/C++ headers using Company
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :defer t
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                  (add-to-list 'company-backends 'company-c-headers)))))

;; company-mode completion back-end for irony-mode
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :defer t
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (progn
                  (add-to-list 'company-backends 'company-irony)
                  (irony-mode))))))

;; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/latest/
;; Requires: `pip install jedi`
;; Company backend for Python jedi
;; https://github.com/syohex/emacs-company-jedi
(use-package company-jedi
  :defer t
  :ensure t
  :init
  (setq-default
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0.2))

(use-package company-lua :defer t :ensure t)

(use-package company-tern :defer t :ensure t)

(use-package css-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'skewer-css-mode))

;; diff-hl - highlight changes/diffs
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(use-package diminish :ensure t)

;; A minor mode that guesses the indentation offset originally used for
;; creating source code files and transparently adjusts the corresponding
;; settings in Emacs, making it more convenient to edit foreign files
;; https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :ensure t
  :config
  (setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1))

(use-package dockerfile-mode :defer t :ensure t)

;; ENSIME brings Scala and Java IDE-like features to your favourite text editor
;; http://ensime.github.io/
;; Java compatibility is still being worked on but is now quite good!
(use-package ensime :defer t :ensure t)

;; Emacs Package Library
;; https://github.com/cask/epl
(use-package epl :ensure t)

(use-package fish-mode :ensure t)

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :ensure t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; Flycheck Status Emoji
;; https://github.com/liblit/flycheck-status-emoji
(use-package flycheck-status-emoji
  :ensure t
  :config
  (flycheck-status-emoji-mode))

;; Shows an inline arguments hint for the C/C++ function at point
;; https://github.com/abo-abo/function-args
(use-package function-args
  :ensure t
  :config
  (fa-config-default)
  :bind
  (:map c-mode-map
        ("M-o" . fa-show))
  (:map c++-mode-map
        ("M-o" . fa-show)))

;; Emacs frontend to GNU Global source code tagging system.
;; https://github.com/leoliu/ggtags
(use-package ggtags
  :diminish ggtags-mode
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))
  :bind
  (:map ggtags-mode-map
        ("C-c g s" . ggtags-find-other-symbol)
        ("C-c g h" . ggtags-view-tag-history)
        ("C-c g r" . ggtags-find-reference)
        ("C-c g f" . ggtags-find-rule)
        ("C-c g c" . ggtags-create-tags)
        ("M-," . pop-tag-mark)))

(use-package gitignore-mode :defer t :ensure t)

(use-package glsl-mode :defer t :ensure t)

;; Golang -- to use or not to use?
(let ((use-go (getenv "EMACS_GO")))
  (if (not (equal use-go nil))
      (load "~/.emacs.d/extra/go")))

(use-package groovy-mode :defer t :ensure t)

(use-package html-mode
  :defer t
  :init
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
(use-package ido :ensure t :config (ido-mode t))

;; A C/C++ minor mode for Emacs powered by libclang
;; https://github.com/Sarcasm/irony-mode
(use-package irony
  :defer t
  :diminish abbrev-mode irony-mode
  :ensure t
  :config
  (defun my-irony-mode-hook ()
    ;; (let ((cmd
    ;;        ;; Build command stolen from irony.el:
    ;;        (format
    ;;         (concat "%s %s %s && %s --build . "
    ;;                 "--use-stderr --config Release --target install")
    ;;         (shell-quote-argument irony-cmake-executable)
    ;;         (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
    ;;                                       (expand-file-name
    ;;                                        irony-server-install-prefix)))
    ;;         (shell-quote-argument irony-server-source-dir)
    ;;         (shell-quote-argument irony-cmake-executable))))

    ;;   ;; Install irony-server if need be
    ;;   (if (not (file-exists-p (concat dot-emacs "/irony/bin/irony-server")))
    ;;       (irony-install-server cmd)))

    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package java-mode
  :init
  (add-hook 'java-mode-hook 'ensime))


(use-package jinja2-mode :defer t :ensure t)

(use-package json-mode :defer t :ensure t)

(use-package lua-mode :defer t :ensure t)

;; A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :ensure t
  :bind
  ("C-c g d" . magit-diff)
  ("C-x g" . magit-status))

(use-package makefile-mode
  :init
  (add-hook 'makefile-gmake-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package markdown-mode
  :ensure t
  :config
  ;; Special indent for markdown-mode
  (add-hook 'markdown-mode-hook
            (global-set-key (kbd "TAB") 'md-indent))
  (defun md-indent ()
    "Indent for `markdown-mode', to be used to rebind TAB - WIP."
    (interactive)
    (if mark-active
        (do-func-to-marked-region 'markdown-indent-region)
      (markdown-indent-line))))

(use-package markdown-mode+ :defer t :ensure t :functions markdown-indent-line)

(use-package nginx-mode :defer t :ensure t)

;; Navigate Python documentation
;; https://github.com/statmobile/pydoc
(use-package pydoc :defer t :ensure t)

(use-package python-mode
  ;; TODO: helper functions that install pip packages
  ;; pip install flake8 jedi
  :bind
  ("<S-down-mouse-1>" . goto-definition-at-point)
  ("<S-down-mouse-3>" . quick-pydoc)
  :functions jedi:goto-definition jedi:stop-server maybe-stop-jedi-server
  :init
  (setq-default python-shell-completion-native-enable nil)

  (defun goto-definition-at-point (event)
    "Move the point to the clicked position
     and jedi:goto-definition the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (jedi:goto-definition)))

  (defun maybe-stop-jedi-server ()
    "Stop the Jedi server, if need me."
    (if (boundp 'jedi:stop-server)
        (jedi:stop-server)))

  (defun quick-pydoc (event)
    "Move the point to the clicked position
     and pydoc the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (pydoc-at-point)))

  (defun use-pyenv3.5.3 ()
    "Configure Jedi to use a pyenv-provided Python 3.5.3."
    (interactive)
    (maybe-stop-jedi-server)
    (let ((pyenv3.5.3 (concat my-home "/.pyenv/versions/3.5.3")))
      (setq
       flycheck-python-flake8-executable "~/.pyenv/versions/3.5.3/bin/flake8"
       jedi:environment-virtualenv (list (concat pyenv3.5.3 "/bin/pyvenv-3.5"))
       jedi:environment-root (concat dot-emacs "/.py/3.5.3")
       jedi:server-args
       ;; TODO: de-hardcode this
       '("--sys-path" "~/.pyenv/versions/3.5.3/lib/python3.5/site-packages"))
      (if (not (file-exists-p
                (concat jedi:environment-root
                        "/lib/python3.5/site-packages/jediepcserver.py")))
          (jedi:install-server))))

  (defun use-pyenv2.7.13 ()
    "Configure Jedi to use a pyenv-provided Python 2.7.13."
    (interactive)
    ;; TODO: pip install virtualenv if not already there
    (maybe-stop-jedi-server)
    (let ((pyenv2.7.13 (concat my-home "/.pyenv/versions/2.7.13")))
      (setq
       flycheck-python-flake8-executable "~/.pyenv/versions/2.7.13/bin/flake8"
       jedi:environment-virtualenv (list (concat pyenv2.7.13 "/bin/virtualenv"))
       jedi:environment-root (concat dot-emacs "/.py/2.7.13")
       jedi:server-args
       ;; TODO: de-hardcode this
       '("--sys-path" "~/.pyenv/versions/2.7.13/lib/python2.7/site-packages"))
      (if (not (file-exists-p
                (concat jedi:environment-root
                        "/lib/python2.7/site-packages/jediepcserver.py")))
          (jedi:install-server))))

  (add-hook 'python-mode-hook 'use-pyenv3.5.3)
  (add-hook 'python-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (add-to-list 'company-backends 'company-jedi)))))

;; GNU Emacs major modes for Racket: Edit and REPL.
;; https://github.com/greghendershott/racket-mode
;; See the below link for why the REPL doesn't load some functions.
;; http://stackoverflow.com/a/31523545
(use-package racket-mode :defer t :ensure t)

;; Start racket-mode via a hook so we get rainbow delimiters
(use-package racket-repl-mode
  :defer t
  :init
  ;; TODO: Currently lacking a good way to start racket-mode when we launch
  ;; TODO: racket-repl on its own, not from a file with C-c C-c.
  ;; (add-hook 'racket-repl-mode-hook (racket-mode))
  (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode))

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow mode - #000 #fff #f00 #ff0 #00f #0f0 #800080 #00ffff #ff00ff
;; https://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :diminish rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Robe: Code navigation, documentation lookup and completion for Ruby
;; https://github.com/dgutov/robe
;; Requires: `gem install pry` and a Gemfile listing your gems
(use-package robe
  :diminish robe-mode
  :ensure t
  :init
  (add-to-list 'exec-path (concat my-home "/.rbenv/shims"))
  (add-to-list 'exec-path (concat my-home "/.rbenv/bin"))
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

;; ruby-dev.el - https://github.com/Mon-Ouie/ruby-dev.el
(use-package ruby-dev
  :ensure t
  :config
  (autoload 'turn-on-ruby-dev "ruby-dev" nil t)
  (add-hook 'ruby-mode-hook 'turn-on-ruby-dev))

;; Activate Robe and company-robe when we start ruby-mode
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (when (derived-mode-p 'ruby-mode)
                (add-to-list 'company-backends 'company-robe))))
  (add-hook 'ruby-mode-hook 'robe-start))

;; Rust -- to use or not to use?
(let ((use-rust (getenv "EMACS_RUST")))
  (if (not (equal use-rust nil))
      (load "~/.emacs.d/extra/rust")))

;; Provides language-aware editing commands based on source code parsers.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
(use-package semantic
  :functions global-semanticdb-minor-mode global-semantic-idle-scheduler-mode
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; skewer-mode: https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :defer t
  :ensure t
  :init
  (setq-default httpd-root (concat dot-emacs "/httpd"))
  :bind
  ("C-c h p" . httpd-start)
  ("C-c h s" . httpd-stop))

;; SLIME company goodness
;; https://github.com/anwyn/slime-company
(use-package slime-company :defer t :ensure t)

;; Superior LISP Interaction Mode for Emacs
;; https://common-lisp.net/project/slime/
;; Cool keybindings to remember:
;; C-c C-c Invoke slime-compile-defun
;; C-c C-l Load current buffer with slime-load-file
;; C-c C-k Compile and load current buffer
;; C-c C-q Invoke slime-close-parens-at-point
(use-package slime
  :ensure t
  :config
  ;; This breaks the default coloring of SLIME.  Net gain in my opinion.
  (add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy)
        slime-net-coding-system 'utf-8-unix))

;; A powerful and beautiful mode-line for Emacs.
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq
   sml/shorten-directory t
   sml/theme 'respectful)
  (sml/setup))

;; Minor mode for Emacs that deals with parens
;; pairs and tries to be smart about it
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :config
  (setq
   sp-base-key-bindings 'paredit
   sp-autoskip-closing-pair 'always
   sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  ;; Fix usage of ' in Lisp modes
  ;; THANKS: https://github.com/Fuco1/smartparens/issues/286#issuecomment-32324743
  ;; (eval) is used as a hack to quiet Flycheck errors about (sp-with-modes)
  (eval
   '(sp-with-modes sp-lisp-modes
      ;; disable ', it's the quote character!
      (sp-local-pair "'" nil :actions nil)
      ;; also only use the pseudo-quote inside strings where it serve as
      ;; hyperlink.
      (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
      (sp-local-pair "`" nil
                     :skip-match (lambda (ms mb me)
                                   (cond
                                    ((equal ms "'")
                                     (or (sp--org-skip-markup ms mb me)
                                         (not (sp-point-in-string-or-comment))))
                                    (t (not (sp-point-in-string-or-comment))))))))
  ;; Don't pair { in web-mode
  (eval
   '(sp-with-modes 'web-mode
      (sp-local-pair "\{" nil :actions nil))))

;;  Emacs isearch with an overview. Oh, man!
;; https://github.com/abo-abo/swiper
(use-package swiper :ensure t)

;; I strongly dislike systemd...
;; but this mode is pretty handy when you need it.
(use-package systemd :defer t :ensure t)

;; Tern: Intelligent JavaScript tooling http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :ensure t
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (progn
                (add-to-list 'exec-path (concat my-home "/.node_modules/tern/bin"))
                (add-to-list 'company-backends 'company-tern)
                (tern-mode t)))))

;; undo-tree.el --- Treat undo history as a tree
;; http://www.dr-qubit.org/undo-tree/undo-tree.el
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq
   ;; TODO: make a special dir for these.
   ;; undo-tree-auto-save-history t
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; web-mode: An autonomous emacs major-mode for editing web templates.
;; http://web-mode.org/
(use-package web-mode
  :defer t
  :ensure t
  :init
  (setq
   web-mode-code-indent-offset 2
   web-mode-comment-style 2
   web-mode-css-indent-offset 2
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-markup-indent-offset 2)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.tpl\\'" . web-mode))

;; windmove: http://is.gd/63r6U0
(use-package windmove
  :bind
  ("M-e" . windmove-left)
  ("M-u" . windmove-right)
  ("M-k" . windmove-up)
  ("M-j" . windmove-down))

;; Unobtrusively trim extraneous whitespace *ONLY* in lines edited
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package yaml-mode :defer t :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (derived-mode-p 'yaml-mode)
                (add-to-list 'company-backends 'company-ansible)))))

;; Yet another snippet extension
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :init
  ;; Shut yas up! Disables startup noise
  (setq-default yas-verbosity 0)
  :config
  (yas-global-mode 1))

;; zygospore: Reversible C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(use-package zygospore
  :ensure t
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Color Theme for emacs based on material design colors
;; https://github.com/cpaulik/emacs-material-theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;; Tweaks to Emacs internals

;; ...But with regular coloring; no highlighting
(defvar whitespace-style
  '(spaces tabs newline space-mark tab-mark newline-mark face))

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(global-auto-revert-mode t)
;; https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode t)
;; No toolbar, please.
(tool-bar-mode -1)
;; Or menu bar...
(menu-bar-mode -1)
;; Or scroll bar.
(scroll-bar-mode -1)
;; Delete highlighted text when you type
(delete-selection-mode t)

(setq
 ;; Backup files ...
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 ;; Show column numbers
 column-number-mode t
 ;; Default startup frame dimensions
 default-frame-alist (add-to-list 'default-frame-alist '(width . 110))
 default-frame-alist (add-to-list 'default-frame-alist '(height . 45))
 ;; Auto-open symlinks that point to vcs-controlled files
 vc-follow-symlinks t
 ;; No splash screen.
 inhibit-splash-screen t
 ;; No default scratch
 initial-scratch-message nil
 ;; Jive with the system clipboard
 select-enable-clipboard t
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 shell-file-name (executable-find "bash")
 xterm-mouse-mode t)

;; Load any custom stuff
(if (file-exists-p custom-file)
    (load-file custom-file))

;; Which browser to open links in
(setq-default
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox"
 ;; display-time-mode options
 display-time-24hr-format t
 display-time-format "%T"
 display-time-interval 1
 ;; No tabs
 indent-tabs-mode nil
 ;; "Tabs" are 4 spaces
 tab-width 4)

(display-time-mode)

;; Enable the disabled things
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'upcase-word 'disabled nil)

;; Always use global-linum-mode and global-hl-line-mode
;; so we can use them when Emacs is running as a daemon
;; http://is.gd/Mw5KiS
(global-linum-mode t)

;; Hack - http://sourcefoundry.org/hack/
(if (or (file-exists-p (concat my-home "/.fonts/Hack-Regular.ttf"))
        (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
    (set-face-attribute 'default nil
                        :family "Hack"
                        :height 100
                        :weight 'normal))
(if (string-equal system-type "darwin")
    (set-face-attribute 'default nil
                        :family "Hack"
                        :height 120
                        :weight 'normal))

;; Symbola - http://zhm.github.io/symbola/
(if (or (file-exists-p (concat my-home "/.fonts/Symbola.ttf"))
        (file-exists-p "/usr/share/fonts/TTF/Symbola.ttf"))
    (set-fontset-font t 'symbol (font-spec :family "Symbola") (selected-frame) 'prepend)
  ;; Again -- proably working on a Mac right now ...
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") (selected-frame) 'prepend))

;;; Define some handy functions

(defun build-project ()
  "Compile the current project."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(defun do-func-to-marked-region (func)
  "Do (FUNC) on a region forward and in reverse."
  (let ((mark (mark))
        (point (point)))
    (if (> mark point)
        (funcall func point mark)
      (funcall func mark point))))

(defun indent-appropriately ()
  "Appropriately indent the current line or region."
  (interactive)
  (if mark-active
      (do-func-to-marked-region 'indent-region)
    (indent-according-to-mode)))

(defun toggle-comment ()
  "Toggle comments on the current line or highlighted region."
  (interactive)
  (if mark-active
    (do-func-to-marked-region 'comment-or-uncomment-region)
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position))))

;;; Rebind/Set several useful keybindings

;; Insert a newline, then indent according to major mode
(global-set-key (kbd "RET") 'newline-and-indent)
;; Build, compile, that stuff
(global-set-key (kbd "<f5>") 'build-project)
;; Make undo work like other editors
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "M-z") 'undo)
;; Better comment-toggling
(global-set-key (kbd "M-/") 'toggle-comment)
;; Make text bigger
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "<f9>") 'text-scale-increase)
;; Or make it smaller
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "<f8>") 'text-scale-decrease)
;; Toggle whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; Extra keybindings that make life great
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "<f13>") 'rgrep) ;; Nice for macs
(global-set-key (kbd "C-x r b") 'revert-buffer)
(global-set-key (kbd "C-c q q q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c u w") 'upcase-word)
(global-set-key (kbd "C-x u") 'upcase-region)
(global-set-key (kbd "C-x t m") 'menu-bar-mode)
(global-set-key (kbd "TAB") 'indent-appropriately)

;; Handy default bindings that are kept:
;; M-< Start of file
;; M-> End of file

;; Kill this buffer!
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

;;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
(let ((fireplace-el (concat my-src "/emacs-fireplace/fireplace.el")))
  (let ((fireplace-elc (concat fireplace-el "c")))
    (if (file-exists-p fireplace-elc)
        (load fireplace-elc)
      (if (file-exists-p fireplace-el)
          (progn
            (byte-compile-file fireplace-el)
            (load fireplace-elc))
        (message (concat "Could not find " fireplace-el " or " fireplace-elc "!!!!"))))))

;; How long did we take to load?
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
