;;; init.el --- Self-installing, for Emacs 24.4+

;;; Commentary:

;; I've tweaked Emacs to behave like "regular" editors in many ways; Ctrl-z
;; for undo, Ctrl-/ to toggle comments on either a line or in a region.
;; It's a nice blend of familiar keybindings on top of Emacs.  This file
;; should work on modern Linux and Mac OSX systems.  I've cultivated this
;; file based on cool things I've seen others do -- huge thanks to all
;; you other Emacs users out there!

;;; Code:

;; Start a timer
(defconst emacs-start-time (current-time))

;; Some initial package stuff
(require 'package)
(setq package-archives
      '(
        ;; GNU over SSL
        ("gnu" . "https://elpa.gnu.org/packages/")
        ;; MELPA (Milkypostman’s Emacs Lisp Package Archive)
        ("melpa" . "https://melpa.org/packages/")
        ;; MELPA Stable
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

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

(use-package apache-mode :defer t :ensure t)

(use-package auto-complete :ensure t)

(use-package auto-complete-clang-async :ensure t)

(use-package autorevert :diminish auto-revert-mode)

;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C,
;; Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode :defer t)

;; CIDER is a Clojure Interactive Development Environment that Rocks for Emacs
;; https://github.com/clojure-emacs/cider
;; Depends on clojure-mode:
;; https://github.com/clojure-emacs/clojure-mode
(use-package cider
  :ensure t
  :bind
  ("C-c n c" . delete-nrepl)
  :config
  (defun delete-nrepl ()
    "Close nREPL connection and delete the window."
    (interactive)
    (cider--close-connection-buffer (current-buffer))
    (delete-window))
  :pin melpa-stable)

;; Clean auto-indent and backspace unindent
;; https://github.com/pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; yasnippet 0.7.0+ snippets for clojure
;; https://github.com/mpenet/clojure-snippets
(use-package clojure-snippets :defer t :ensure t)

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :ensure t)

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
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

;; company-mode backend for emoji
;; https://github.com/dunn/company-emoji
(use-package company-emoji
  :defer t
  :ensure t)

 ;; Company backend for Python jedi - https://github.com/syohex/emacs-company-jedi
(use-package company-jedi
  :defer t
  :ensure t)

;; company-mode autocompletion for golang
;; https://github.com/nsf/gocode/tree/master/emacs-company
(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  ;; Set up environment variables so Flycheck can find gocode.
  (setenv "GOPATH" (concat (getenv "HOME") "/src/golibs"))
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))

;;  Company integration for racer
;; https://github.com/emacs-pe/company-racer
(use-package company-racer :ensure t)

;; A major-mode for editing C# in emacs
;; https://github.com/josteink/csharp-mode
(use-package csharp-mode :defer t :ensure t)

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

;; elpy: the Emacs Lisp Python Environment
;; https://github.com/jorgenschaefer/elpy
;; Requires: `pip install elpy`
(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python (elpy-enable))
  ;; Change Python versions, on the fly
  (defun use-pyenv-python351 ()
    "Point to Python 3.5.1 for `elpy-mode', `flycheck-mode', and `python-mode'."
    (interactive)
    (let ((pyenv-351 (concat (getenv "HOME") "/.pyenv/versions/3.5.1")))
      (setq
       elpy-rpc-python-command (concat pyenv-351 "/bin/python3.5m")
       elpy-rpc-pythonpath (concat pyenv-351 "/lib/python3.5/site-packages")
       flycheck-python-flake8-executable (concat pyenv-351 "/bin/flake8")
       python-check-command (concat pyenv-351 "/bin/pyflakes")
       python-shell-interpreter (concat pyenv-351 "/bin/ipython3"))))

  (defun use-pyenv-python344 ()
    "Point to Python 3.4.4 for `elpy-mode', `flycheck-mode', and `python-mode'."
    (interactive)
    (let ((pyenv-344 (concat (getenv "HOME") "/.pyenv/versions/3.4.4")))
      (setq
       elpy-rpc-python-command (concat pyenv-344 "/bin/python3.4m")
       elpy-rpc-pythonpath (concat pyenv-344 "/lib/python3.4/site-packages")
       flycheck-python-flake8-executable (concat pyenv-344 "/bin/flake8")
       python-check-command (concat pyenv-344 "/bin/pyflakes")
       python-shell-interpreter (concat pyenv-344 "/bin/ipython3"))))

  (defun use-pyenv-python2 ()
    "Point to Python 2 for `elpy-mode', `flycheck-mode', and `python-mode'."
    (interactive)
      (let ((pyenv-2711 (concat (getenv "HOME") "/.pyenv/versions/2.7.11")))
        (setq
         elpy-rpc-python-command (concat pyenv-2711 "/bin/python2.7")
         elpy-rpc-pythonpath (concat pyenv-2711 "/lib/python2.7/site-packages")
         flycheck-python-flake8-executable (concat pyenv-2711 "/bin/flake8")
         python-check-command (concat pyenv-2711 "/bin/pyflakes")
         python-shell-interpreter (concat pyenv-2711 "/bin/ipython"))))

  (defun use-system-python34 ()
    "Use the system python3 for `elpy-mode', `flycheck-mode', and `python-mode'."
    (interactive)
    (setq
     elpy-rpc-python-command "/usr/bin/python3.4m"
     elpy-rpc-pythonpath "/usr/lib/python3.4/site-packages"
     flycheck-python-flake8-executable "/usr/bin/flake8"
     python-check-command "/usr/bin/pyflakes"
     python-shell-interpreter "/usr/bin/ipython3.4"))

  (defun use-system-python2 ()
    "Use the system python2 for `elpy-mode', `flycheck-mode', and `python-mode'."
    (interactive)
    (setq
     elpy-rpc-python-command "/usr/bin/python2.7"
     elpy-rpc-pythonpath "/usr/local/lib/python2.7/dist-packages"
     flycheck-python-flake8-executable "/usr/bin/flake8"
     python-check-command "/usr/bin/pyflakes"
     python-shell-interpreter "/usr/bin/ipython"))

  ;; Default
  (use-pyenv-python351)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (add-to-list 'company-backends 'company-jedi)))))

;; EMMS - https://www.gnu.org/software/emms/
;; TODO: https://www.gnu.org/software/emms/manual/#Track-Information
(use-package emms
  :bind
  (("<f1>" . emms-volume-lower)
   ("<f2>" . emms-volume-raise)
   ("<f3>" . emms-previous)
   ("<f4>" . emms-next)
   ("<f6>" . emms-play-directory)
   ("<f7>" . emms-pause)
   ("C-c m SPC" . emms-pause)
   ("C-c m a" . emms-add-directory)
   ("C-c m b" . emms-previous)
   ("C-c m f" . emms-next)
   ("C-c m l" . emms-lyrics-toggle)
   ("C-c m n" . emms-playlist-mode-go)
   ("C-c m p" . emms-start)
   ("C-c m q" . emms-shuffle)
   ("C-c m s" . emms-stop)
   ("C-c m v d" . emms-volume-lower)
   ("C-c m v u" . emms-volume-raise)
   ("C-c p b" . mpd-rev10)
   ("C-c p c" . emms-player-mpd-connect)
   ("C-c p d" . emms-player-mpd-disconnect)
   ("C-c p f" . mpd-seek10)
   ("C-c p p" . emms-player-mpd-previous)
   ("C-c p n" . emms-player-mpd-next)
   ("C-c p s" . emms-player-mpd-show)
   ("C-x t e" . emms-mode-line-toggle))
  :ensure t
  :init
  (defun mpd-rev10 ()
    "Seek backward ten seconds."
    (interactive)
    (emms-player-mpd-seek -10))
  (defun mpd-seek10 ()
    "Seek forward ten seconds."
    (interactive)
    (emms-player-mpd-seek 10))
  :config
  (progn
    (require 'emms-mode-line-cycle)
    (require 'emms-mode-line-icon)
    (require 'emms-player-mpv)
    (emms-add-directory-tree (concat (getenv "HOME") "/music"))
    (emms-mode-line 1)
    (emms-mode-line-cycle 1)
    (emms-playing-time 1)
    (setq
     emms-mode-line-cycle-additional-space-num 4
     emms-mode-line-cycle-any-width-p t
     emms-mode-line-cycle-current-title-function
     (lambda ()
       (let ((track (emms-playlist-current-selected-track)))
         (cl-case (emms-track-type track)
           ((streamlist)
            (let ((stream-name (emms-stream-name
                                (emms-track-get track 'metadata))))
              (if stream-name stream-name (emms-track-description track))))
           ((url) (emms-track-description track))
           (t (file-name-nondirectory
               (emms-track-description track))))))
     emms-mode-line-cycle-max-width 25
     emms-mode-line-cycle-use-icon-p t
     emms-mode-line-cycle-velocity 2
     emms-mode-line-format " ( %s )"
     emms-mode-line-titlebar-function
     (lambda ()
       '(:eval
         (when emms-player-playing-p
           (format " %s %s"
                   (format emms-mode-line-format (emms-mode-line-cycle-get-title))
                   emms-playing-time-string))))
     emms-player-list '(emms-player-mpv)
     emms-source-file-default-directory (concat (getenv "HOME") "/music"))))

;; Display the emms mode line as a ticker
;; https://github.com/momomo5717/emms-mode-line-cycle
(use-package emms-mode-line-cycle
  :defer t
  :ensure t)

;; mpv support for EMMS - https://github.com/dochang/emms-player-mpv/
(use-package emms-player-mpv
  :defer t
  :ensure t)

;; Emacs Package Library
;; https://github.com/cask/epl
(use-package epl :ensure t)

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :ensure t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-irony :ensure t)

(use-package flycheck-rust :ensure t)

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
  :ensure t
  :config
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

(use-package go-mode :defer t :ensure t)

(use-package groovy-mode :defer t :ensure t)

;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :ensure t
  :config
  (ido-mode t))

;; Python auto-completion for Emacs
;; http://tkf.github.io/emacs-jedi/latest/
;; Requires: `pip install jedi`
(use-package jedi
  :defer t
  :ensure t
  :init
  (setq jedi:setup-keys 1))

(use-package jinja2-mode :defer t :ensure t)

(use-package js2-highlight-vars :ensure t)

(use-package js2-mode :defer t :ensure t)

(use-package json-mode :defer t :ensure t)

(use-package lua-mode :defer t :ensure t)

;; https://github.com/andre-richter/emacs-lush-theme
;; Other cool themes: atom-one-dark, abyss, darcula, gotham, obsidian
(use-package lush-theme :ensure t
  :config
  (load-theme 'lush t))

(use-package magit :ensure t
  :bind
  ("C-c g d" . magit-diff)
  ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t
  :config
  ;; Special indent for markdown-mode
  (add-hook 'markdown-mode-hook
            (global-set-key (kbd "TAB") 'md-indent))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when (derived-mode-p 'markdown-mode)
                (add-to-list 'company-backends 'company-emoji))))
  (defun md-indent ()
    "Indent for `markdown-mode', to be used to rebind TAB - WIP."
    (interactive)
    (if mark-active
        (do-func-to-marked-region 'markdown-indent-region)
      (markdown-indent-line))))

(use-package markdown-mode+ :defer t :ensure t)

(use-package nginx-mode :defer t :ensure t)

;; Run jedi when we use python-mode
(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

;; rust-mode: https://github.com/rust-lang/rust-mode
;; and emacs-racer: https://github.com/racer-rust/emacs-racer
(use-package racer
  :ensure t
  :init
  (defvar racer-cmd (concat (getenv "HOME") "/src/racer/target/release/racer"))
  (defvar racer-rust-src-path (concat (getenv "HOME") "/src/rust/src"))
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

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
  (add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/shims"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/bin"))
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

;; Provides language-aware editing commands based on source code parsers.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1)
  ;; add moar include paths like this ...
  ;; (semantic-add-system-include "/usr/include/boost" 'c++-mode)
  ;; (semantic-add-system-include (concat (getenv "HOME") "/linux/kernel"))
  ;; (semantic-add-system-include (concat (getenv "HOME") "/linux/include"))
  )

;; skewer-mode: https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :defer t
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; TODO
;; SLIME: The Superior Lisp Interaction Mode for Emacs
;; https://common-lisp.net/project/slime/
;; (require 'slime)
;; (setq
;;  inferior-lisp-program "/usr/bin/sbcl"
;;  slime-contribs '(slime-fancy))

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
  (smartparens-global-mode 1))

;; Smart Tabs (indenting with tabs and aligning with spaces)
;; http://www.emacswiki.org/emacs/SmartTabs
;; (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python
;;                       'ruby 'nxml)

;; https://www.emacswiki.org/emacs/SrSpeedbar
(use-package sr-speedbar :ensure t)

;;  Emacs isearch with an overview. Oh, man!
;; https://github.com/abo-abo/swiper
(use-package swiper :ensure t)

;; web-mode: An autonomous emacs major-mode for editing web templates.
;; http://web-mode.org/
(use-package web-mode
  :defer t
  :ensure t
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html\\'" . web-mode)
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

(use-package yaml-mode :defer t :ensure t)

;; Yet another snippet extension
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; zygospore: Reversible C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(use-package zygospore
  :ensure t
  :bind
  ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Tweaks to Emacs internals

;; Always show whitespace characters
;; (global-whitespace-mode)

;; ...But with regular coloring; no highlighting
(defvar whitespace-style
  '(spaces tabs newline space-mark tab-mark newline-mark face))

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(global-auto-revert-mode t)

;; No toolbar, please.
(tool-bar-mode -1)
;; Or menu bar...
(menu-bar-mode -1)
;; Or scroll bar.
(scroll-bar-mode -1)
;; Delete highlighted text when you type
(delete-selection-mode t)

(setq
 ;; Show column numbers
 column-number-mode t
 ;; gdb-many-windows t
 ;; gdb-show-main t
 ;; Auto-open symlinks that point to vcs-controlled files
 vc-follow-symlinks t
 ;; No splash screen.
 inhibit-splash-screen t
 ;; No default scratch
 initial-scratch-message nil
 ;; Jive with the system clipboard
 x-select-enable-clipboard t)

(setq-default
 ;; No tabs
 indent-tabs-mode nil
 ;; "Tabs" are 4 spaces
 tab-width 4)

;; Enable the disabled things
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'upcase-word 'disabled nil)

;; Use hl-line-mode and linum-mode only if we
;; are not using a character-based terminal
(if (display-graphic-p)
    (progn
      ;; http://is.gd/Mw5KiS
      (global-linum-mode t)
      ;; http://is.gd/4jOQ8Y
      (global-hl-line-mode 1)))

;; Hack - http://sourcefoundry.org/hack/
(if (or (file-exists-p (concat (getenv "HOME") "/.fonts/Hack-Regular.ttf"))
        (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
    (set-face-attribute 'default nil
                        :family "Hack"
                        :height 100
                        :weight 'normal)
  ;; Probably on a Mac ...
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height 120
                      :weight 'normal))

;; Symbola - http://zhm.github.io/symbola/
(if (or (file-exists-p (concat (getenv "HOME") "/.fonts/Symbola.ttf"))
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

;;; Rebind/Set several useful keybindings - many of which make Emacs behave like
;;; other (not vim) editors.

;; UNUSED: None!

;; Insert a newline, then indent according to major mode
(global-set-key (kbd "RET") 'newline-and-indent)
;; Build, compile, that stuff
(global-set-key (kbd "<f5>") 'build-project)
;; Make undo work like other editors
(global-set-key (kbd "C-z") 'undo)
;; Better comment-toggling
(global-set-key (kbd "C-/") 'toggle-comment)
;; Make text bigger
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "<f9>") 'text-scale-increase)
;; Or make it smaller
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<f8>") 'text-scale-decrease)
;; Toggle whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; Extra keybindings that make life great
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "<f13>") 'rgrep)
(global-set-key (kbd "C-c s c") 'slime-connect)
(global-set-key (kbd "C-x r b") 'revert-buffer)
(global-set-key (kbd "C-c u w") 'upcase-word)
(global-set-key (kbd "C-x u") 'upcase-region)
(global-set-key (kbd "C-x t m") 'menu-bar-mode)
(global-set-key (kbd "TAB") 'indent-appropriately)
(global-set-key (kbd "<f12>") 'fireplace)

;; Kill this buffer!
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

;;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
(let ((fireplace-el (concat (getenv "HOME") "/src/emacs-fireplace/fireplace.el")))
  (let ((fireplace-elc (concat fireplace-el "c")))
    (if (file-exists-p fireplace-elc)
        (load fireplace-elc)
      (if (file-exists-p fireplace-el)
          (progn
            (byte-compile-file fireplace-el)
            (load fireplace-elc))
        (message (concat "Could not find " fireplace-el " or " fireplace-elc "!!!!"))))))

;; How long did we take to load?
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed)))

(provide 'init)
;;; init.el ends here
