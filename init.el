;;; init.el --- Self-installing, for Emacs 25 and up.
;;; Commentary:
;; My Emacs configuration for writing code and things.  Might work on Emacs 24.
;; Below is a keybindings cheat-sheet, many of these are defaults.
;;
;; Capitalization bindings:
;; M-c		Capitalize first letter of word
;; C-c u w	Uppercase word
;; C-c d w	Lowercase word
;; C-x u	Uppercase region
;;
;; Delete bindings:
;; C-d		Delete character under the cursor
;; M-d		Delete next word
;; M-Del	Delete previous word
;; C-k		Delete from cursor to end of line
;; M-k		Delete next sentence
;; C-x Del	Delete previous sentence
;; C-y		Restore deleted thing
;;
;; Emacs bindings:
;; C-c q q q	Save all buffers and close Emacs (useful for daemon mode)
;;
;; Fireplace bindings:
;; d	Move the fireplace up
;; s	Toggle smoke
;; u	Move the fireplace down
;;
;; Lisp bindings:
;; TODO
;;
;; Mark and region commands:
;; C-x C-x	Swap location of cursor and mark
;; C-w		Kill region
;; C-y		Paste most recently killed or copied text
;; M-w		Copy region
;; M-h		Mark paragraph
;; C-x C-p	Mark page
;; C-x h	Mark buffer
;;
;; Navigation bindings:
;; C-f		Move forward (right) one character
;; C-b		Move backward (left) one character
;; C-p		Move to previous line (up)
;; C-n		Move to next line (down)
;; M-f		Move one word forward (right)
;; M-b		Move one word backward (left)
;; C-a		Move to the beginning of the current line
;; C-e		Move to the end of the current line
;; M-e		Move forward (right) one sentence
;; M-a		Move backward (left) one sentence
;; M-}		Move forward (right) one paragraph
;; M-{		Move backward (left) one paragraph
;; C-v		Move forward (down) one screen
;; M-v		Move backward (up) one screen
;; C-x ]	Move forward one page
;; C-x [	Move backward one page
;; M-<		Start of file
;; M->		End of file
;; C-l		Redraw screen with current line in center.  Run twice to push
;;			current line to top, three times to push it to the bottom.
;; M-$n		Repeat the next command $n times
;; C-u $n	Repeat the next command $n times (or four times if $n is omitted)
;;
;; Org mode bindings:
;;
;; C-c C-o				org-open-at-point
;; C-c C-s				org-schedule
;; C-c C-t				org-todo
;; C-c a a				org-agenda
;; C-c a t				Enter global TODO list (requires org-agenda-files be set)
;; C-c l				org-store-link
;; M-S-RET				org-insert-todo-heading
;; S-TAB				Toggle overview/folding
;;
;; Org mode agenda bindings (http://orgmode.org/manual/Agenda-commands.html):
;;
;; A					Interactively select another agenda view and append it to the current view.
;; v d || d				org-agenda-day-view
;; v w || w				org-agenda-week-view
;; v t					org-agenda-fortnight-view
;; v m					org-agenda-month-view
;; v y					org-agenda-year-view
;; v SPC				org-agenda-reset-view
;; f					Go forward in time to display the following org-agenda-current-span days.
;;                      For example, if the display covers a week, switch to the following week.
;;                      With prefix arg, go forward that many times org-agenda-current-span days.
;; b					Go backward in time to display earlier dates.
;; .					Go to today.
;; j					Prompt for a date and go there.
;; J					Go to the currently clocked-in task in the agenda buffer.
;; D					Toggle the inclusion of diary entries.  See http://orgmode.org/manual/Weekly_002fdaily-agenda.html#Weekly_002fdaily-agenda
;; v l || l				Toggle logbook mode.
;;
;; Rectangle bindings:
;; C-x r k				Delete a rectangle and store it
;; C-x r d				Delete a rectangle and do not store it
;; C-x r y				Insert the last rectangle killed
;; C-x r c				Using spaces, blank out the area marked as a rectangle and do not store it
;; C-x r o				Insert a blank rectangle in the area marked
;; C-x r r r			Copy rectangle from register r (where r is any character)
;; C-x r i r			Insert a rectangle from register r (where r is any character)
;; C-x r t string Enter	Changes contents of marked rectangle to string (if string is narrower or
;;					   	wider than rectangle, dimensions change accordingly)
;;
;; Search bindings:
;; C-s C-w	Start an incremental search with the word the cursor is on as the search string
;; C-s C-y	Start an incremental search with the text from the cursor position to the end of
;;			the line as the search string
;; C-s M-y	Start an incremental search with text from the kill ring as the search string
;; C-s C-s	Repeat previous search
;; C-r C-r	Repeat previous reverse search
;;
;; Window management bindings:
;; C-x 2	Divide current window into two windows, one above the other
;; C-x 3	Divide current window into two side-by-side windows
;; C-x 1	Delete other windows but the current one;  run again to undo
;; C-x k	Kill the current buffer
;; C-x ^	Make the current window taller
;; C-x }	Make the current window wider
;; C-x {	Make the current window narrower
;; C-x -	Make the current window smaller if buffer is smaller than window
;; C-x +	Make windows the same size
;; C-M-v	Scroll other window
;;
;; Window movement bindings:
;; C-x o	Move to the other window; if there are several, move to the next window
;; M-k		Move window up
;; M-u		Move window right
;; M-j		Move window down
;; M-e		Move window left
;;
;; Neat functions that could use some bindings:
;; delete-whitespace-rectangle	If a rectangle includes initial whitespace,
;;								deletes it, narrowing rectangle
;; shrink-window				Make the current window taller
;; string-insert-rectangle		Prompts for string and inserts rectangle
;;
;;; Recommended reading:
;;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
;;;
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
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ;; Org mode ELPA archive
   ("org" . "https://orgmode.org/elpa/")))

;; TODO: This is necessary to trust sml themes, but can
;; the later, redundant loading of custom-file be stopped?
(load custom-file :noerror :nomessage)

;; Pin here because use-package doesn't sseem to be able to...
;; https://github.com/jwiegley/use-package/issues/343
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

;; Ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use some packages (configure them too!)
;; https://github.com/jwiegley/use-package/
(defvar use-package-verbose t)
(defvar use-package-always-ensure t)
(eval-when-compile (require 'use-package))

(require 'bind-key)

;; https://github.com/jwiegley/use-package#use-package-ensure-system-package
(use-package use-package-ensure-system-package)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(use-package autorevert :diminish auto-revert-mode)

;; Tool for capturing screencasts directly from Emacs.
;; https://github.com/Malabarba/camcorder.el
;; (use-package camcorder
;;   :defer t
;;   :config
;;   (custom-set-variables
;;    '(camcorder-frame-parameters
;;      (quote
;;       ((height . 45)
;;        (width . 110))))
;;    '(camcorder-gif-output-directory "~/videos/emacs/gifs")
;;    '(camcorder-output-directory "~/videos/emacs")))

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken :defer t)

;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C,
;; Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :defer t
  :config
  (defun clang-format-save-hook ()
    "Run clang-format on save when in c or c++ mode when the variable
     'clang-forma-on-save' is set. Put the following into a .dir-locals.el file
     in your project to use this:

     ((nil . ((eval . (setq clang-format-on-save t)))))"
    (interactive)
    (when (and (boundp 'clang-format-on-save)
               (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)))
      (clang-format-buffer)))
  (add-hook 'before-save-hook 'clang-format-save-hook)
  ;;
  ;; Add the following to a .dir-locals.el file in your C/C++ project:
  ;;
  ;; ((nil . ((eval . (add-hook 'after-save-hook 'export-compile-commands-foo)))))
  ;;
  ;; This will regenerate the `compile_commands.json' file after each save.
  ;;
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
(use-package clang-format
  :defer t
  :functions clang-format-buffer)

;; Clean auto-indent and backspace unindent
;; https://github.com/pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t)

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :diminish company-mode
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
(use-package company-ansible :defer t)

;; Auto-completion for C/C++ headers using Company
;; https://github.com/randomphrase/company-c-headers
(use-package company-c-headers
  :defer t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                  (add-to-list 'company-backends 'company-c-headers)))))

;; company-mode completion back-end for irony-mode
;; https://github.com/Sarcasm/company-irony
(use-package company-irony
  :defer t
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
  :init
  (setq-default
   jedi:complete-on-dot t
   jedi:get-in-function-call-delay 0.2))

;; company-mode completion backend for Lua
;; https://github.com/ptrv/company-lua
(use-package company-lua :defer t)

;; A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.
;; http://www.rtags.net / https://github.com/Andersbakken/rtags
(use-package company-rtags
  :defer t
  :functions rtags-diagnostics rtags-set-periodic-reparse-timeout
  :config
  (progn
    (setq-default rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq-default rtags-completions-enabled t)
    (push 'company-rtags company-backends)))

;; https://github.com/rafalcieslak/emacs-company-terraform
;; Company backend for terraform files
(use-package company-terraform :defer t)

;; Tern backend for company-mode.
;; https://github.com/proofit404/company-tern
(use-package company-tern :defer t)

;; Various completion functions using Ivy
;; https://github.com/abo-abo/swiper / https://melpa.org/#/counsel
(use-package counsel :defer t)

;; Built into Emacs
(use-package css-mode :init (add-hook 'css-mode-hook 'skewer-css-mode))

;; diff-hl - highlight changes/diffs
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
(use-package diminish)

;; A minor mode that guesses the indentation offset originally used for
;; creating source code files and transparently adjusts the corresponding
;; settings in Emacs, making it more convenient to edit foreign files
;; https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :config
  (setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1))

;; An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode :defer t)

;; Emacs Package Library
;; https://github.com/cask/epl
(use-package epl)

;;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
(use-package fireplace
  :init (defvar fireplace-mode-map)
  :bind (:map fireplace-mode-map
              ("d" . fireplace-down)
              ("s" . fireplace-toggle-smoke)
              ("u" . fireplace-up)))

;; fish-mode for emacs
;; https://github.com/wwwjfy/emacs-fish
(use-package fish-mode)

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; C, C++ and Objective-C support for Flycheck, using Irony Mode
;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck-irony
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.
;; http://www.rtags.net / https://github.com/Andersbakken/rtags
(use-package flycheck-rtags
  :defer t
  :functions flycheck-select-checker setup-flycheck-rtags
  :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0))  ;; Run flycheck 2 seconds after being idle.
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)))

;; Flycheck Status Emoji
;; https://github.com/liblit/flycheck-status-emoji
(use-package flycheck-status-emoji
  :config
  (flycheck-status-emoji-mode))

;; Shows an inline arguments hint for the C/C++ function at point
;; https://github.com/abo-abo/function-args
(use-package function-args
  :functions fa-config-default
  :config
  (fa-config-default)
  :bind
  (:map c-mode-map
        ("M-o" . fa-show))
  (:map c++-mode-map
        ("M-o" . fa-show)))

;; Major mode for editing Godot GDScript files
(use-package gdscript-mode :defer t)

;; TODO: Make this optional.
;; Emacs frontend to GNU Global source code tagging system.
;; https://github.com/leoliu/ggtags
;; (use-package ggtags
;;   :defer t
;;   :diminish ggtags-mode
;;   :init
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
;;                 (ggtags-mode 1))))
;;   :bind
;;   (:map ggtags-mode-map
;;         ("C-c g s" . ggtags-find-other-symbol)
;;         ("C-c g h" . ggtags-view-tag-history)
;;         ("C-c g r" . ggtags-find-reference)
;;         ("C-c g f" . ggtags-find-rule)
;;         ("C-c g c" . ggtags-create-tags)
;;         ("M-," . pop-tag-mark)))


;; Emacs mode for .gitignore files
;; https://github.com/magit/git-modes/blob/master/gitignore-mode.el
(use-package gitignore-mode :defer t)

;; GLSL emacs mode
;; https://github.com/jimhourihan/glsl-mode
(use-package glsl-mode :defer t)

(use-package go-mode
  :ensure-system-package ((go)
                          (goimports . "go get golang.org/x/tools/cmd/goimports"))
  :init
  ;; Set up so that go get doesn't spam $HOME
  (setenv "GOPATH" (concat my-home "/.local/go"))
  ;; Add the resulting bindir to the exec-path
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-eldoc :defer t)

;; A groovy major mode, grails minor mode, and a groovy inferior mode.
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode :defer t)

;; Built into Emacs
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html
(use-package html-mode :no-require t :ensure nil  :init (add-hook 'html-mode-hook 'skewer-html-mode))

;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
(use-package ido :config (ido-mode t))

;; A C/C++ minor mode for Emacs powered by libclang
;; https://github.com/Sarcasm/irony-mode
(use-package irony
  :defer t
  :diminish abbrev-mode irony-mode
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; irony-mode support for eldoc-mode
;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc :defer t)

;; Jinja2 mode for emacs
;; https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode :defer t)

;; Major mode for editing JSON files with emacs
;; https://github.com/joshwnj/json-mode
(use-package json-mode :defer t)

;; Emacs major mode for editing Lua
;; http://immerrr.github.io/lua-mode/
(use-package lua-mode :defer t)

;; A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :bind
  ("C-c g d" . magit-diff-range)
  ("C-x g" . magit-status)
  :config
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show))

;; Emacs Markdown Mode
;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
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

;; Additional functions for Emacs [markdown-mode].
;; https://github.com/milkypostman/markdown-mode-plus
(use-package markdown-mode+ :defer t :functions markdown-indent-line)

;; A Better Java Development Environment for Emacs
;; https://github.com/mopemope/meghanada-emacs
(use-package meghanada :defer t)

;; Emacs editing mode for Nginx config files
;; https://github.com/ajc/nginx-mode
(use-package nginx-mode :defer t)

;; https://orgmode.org/elpa.html
(use-package org-plus-contrib
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :defer t
  :init (setq-default org-startup-truncated nil))

;; Navigate Python documentation
;; https://github.com/statmobile/pydoc
(use-package pydoc :defer t)

;; Built into Emacs
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python-mode
  :bind
  ("<S-down-mouse-1>" . goto-definition-at-point)
  ("<S-down-mouse-3>" . quick-pydoc)
  :ensure nil
  :functions jedi:goto-definition jedi:stop-server maybe-stop-jedi-server pydoc-at-point
  :init

  (defun goto-definition-at-point (event)
    "Move the point to the clicked position
     and jedi:goto-definition the thing at point."
    (interactive "e")
    (let ((es (event-start event)))
      (select-window (posn-window es))
      (goto-char (posn-point es))
      (jedi:goto-definition)))

  (defun maybe-stop-jedi-server ()
    "Stop the Jedi server, if need be."
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

  (defun use-system-python3 ()
    "Use the system python3."
    (interactive)
    (maybe-stop-jedi-server)
    (defvar python-check-command)
    (defvar python-shell-interpreter)
    (setq
       python-check-command "pyflakes"
       python-shell-interpreter "python3"
       flycheck-python-flake8-executable "flake8"
       jedi:environment-virtualenv (list "python3" "-m" "venv")
       jedi:environment-root (concat dot-emacs "/.py/system3")
       jedi:server-args
       '("--sys-path" "/usr/lib/python3.6/site-packages"
         "--sys-path" "~/.local/lib/python3.6/site-packages"))
      (if (not (file-exists-p
                (concat jedi:environment-root
                        "/lib/python3.6/site-packages/jediepcserver.py")))
          (jedi:install-server)))

  (add-hook 'python-mode-hook 'use-system-python3)
  (add-hook 'python-mode-hook 'blacken-mode)
  (add-hook 'python-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (add-to-list 'company-backends 'company-jedi)))))

;; GNU Emacs major modes for Racket: Edit and REPL.
;; https://github.com/greghendershott/racket-mode
;; See the below link for why the REPL doesn't load some functions.
;; http://stackoverflow.com/a/31523545
(use-package racket-mode :defer t)

;; Start racket-mode via a hook so we get rainbow delimiters
(use-package racket-repl-mode
  :defer t
  :ensure nil
  :init
  ;; TODO: Currently lacking a good way to start racket-mode when we launch
  ;; TODO: racket-repl on its own, not from a file with C-c C-c.
  ;; (add-hook 'racket-repl-mode-hook (racket-mode))
  (add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode))

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow mode - #000 #fff #f00 #ff0 #00f #0f0 #800080 #00ffff #ff00ff
;; https://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Robe: Code navigation, documentation lookup and completion for Ruby
;; https://github.com/dgutov/robe
;; Requires: `gem install pry` and a Gemfile listing your gems
(use-package robe
  :diminish robe-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

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
(require 'semantic)
(use-package semantic
  :commands semantic-mode
  :functions (global-semanticdb-minor-mode global-semantic-idle-scheduler-mode)
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; Live web development in Emacs
;; https://github.com/skeeto/skewer-mode
(use-package skewer-mode
  :defer t
  :init
  (setq-default httpd-root (concat dot-emacs "/httpd"))
  :bind
  ("C-c h p" . httpd-start)
  ("C-c h s" . httpd-stop))

;; SLIME company goodness
;; https://github.com/anwyn/slime-company
(use-package slime-company :defer t)

;; Superior LISP Interaction Mode for Emacs
;; https://common-lisp.net/project/slime/
;; Cool keybindings to remember:
;; C-c C-c Invoke slime-compile-defun
;; C-c C-l Load current buffer with slime-load-file
;; C-c C-k Compile and load current buffer
;; C-c C-q Invoke slime-close-parens-at-point
(use-package slime
  :config
  ;; This breaks the default coloring of SLIME.  Net gain in my opinion.
  (add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-company slime-fancy)
        slime-net-coding-system 'utf-8-unix))

;; A powerful and beautiful mode-line for Emacs.
;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
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
  :config
  (setq
   sp-autoskip-closing-pair 'always
   sp-hybrid-kill-entire-symbol nil)
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
(use-package swiper)

;; I strongly dislike systemd... but this mode is pretty handy when you need it.
;; https://github.com/holomorph/systemd-mode
(use-package systemd :defer t)

;; Tern: Intelligent JavaScript tooling
;; http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :commands tern-mode
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (progn
                (add-to-list 'exec-path (concat my-home "/.local/bin"))
                (add-to-list 'company-backends 'company-tern)
                (tern-mode t)))))

;; Major mode of Terraform configuration file
;; https://github.com/syohex/emacs-terraform-mode
(use-package terraform-mode :init (company-terraform-init))


;; Emacs Mojor mode for editing TOML files
;; https://github.com/dryman/toml-mode.el
(use-package toml-mode :defer t)

;; undo-tree.el --- Treat undo history as a tree
;; http://www.dr-qubit.org/undo-tree/undo-tree.el
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
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
  :init
  (setq-default
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

;; windmove, built into Emacs: http://is.gd/63r6U0
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
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;; The emacs major mode for editing files in the YAML data serialization format.
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (derived-mode-p 'yaml-mode)
                (add-to-list 'company-backends 'company-ansible)))))

;; Yet another snippet extension
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  ;; Shut yas up! Disables startup noise
  (setq-default yas-verbosity 0)
  :config
  (yas-global-mode 1))

;; zygospore: Reversible C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;; Color Theme for emacs based on material design colors
;; https://github.com/cpaulik/emacs-material-theme
;; TODO: How to use darker grey vs blue colors?
(use-package material-theme
  :config (load-theme 'material t))

;; Use conf-mode for .godot and .tscn source files
(setq auto-mode-alist (append '(("\\.godot$" . conf-mode))
                              '(("\\.tscn$" . conf-mode))
                              auto-mode-alist))

;; Tweaks to Emacs internals

(defconst default-org-file "~/src/org/home.org")

(defvar whitespace-style
  '(spaces tabs newline space-mark tab-mark newline-mark face))

;; Emacs highlights misspellings only in comments or strings
(flyspell-prog-mode)
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

;; As advised by https://www.emacswiki.org/emacs/TrampMode
(setq-default tramp-default-method "ssh")

(setq
 ;; Backup files ...
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 ;; Show column numbers
 column-number-mode t
 ;; Make org-agenda-list open when Emacs launches. This does not
 ;; work with daemon mode, instead use this arg with emacsclient:
 ;; --eval '(org-agenda-list)'
 ;; initial-buffer-choice 'org-agenda-list
 ;; Auto-open symlinks that point to vcs-controlled files
 vc-follow-symlinks t
 ;; No splash screen.
 inhibit-splash-screen t
 ;; No default scratch
 initial-scratch-message nil
 ;; Show five lines from the previous page when paging up or down
 next-screen-context-lines 5
 ;; Jive with the system clipboard
 select-enable-clipboard t
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 shell-file-name (executable-find "bash")
 xterm-mouse-mode t)

;; Load any custom stuff
(if (file-exists-p custom-file)
    (load-file custom-file))

(setq-default
;; Which browser to open links in
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox"
 ;; display-time-mode options
 display-time-24hr-format t
 display-time-format "%T"
 display-time-interval 1
 ;; https://emacs.stackexchange.com/a/16836
 ;; TODO: When Emacs is iconified/minimized the title reverts to the default.
 ;; When Emacs is iconified/minimized and another Emacs frame is in focus, the
 ;; title becomes just the file name (not the full path.)
 frame-title-format '("%f [%m]")
 ;; Don't automatically 'fix' files with DOS line endings
 inhibit-eol-conversion t
 ;; Org mode stuff
 org-log-done t
 org-agenda-files (list default-org-file)
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

;; http://is.gd/Mw5KiS
(global-linum-mode t)

(defvar h-font-height)
(if (getenv "RETINA_DISPLAY")
    (setq h-font-height 170)
  (setq h-font-height 100))

;; Hack - http://sourcefoundry.org/hack/
(if (or (file-exists-p (concat my-home "/.fonts/Hack-Regular.ttf"))
        (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
    (set-face-attribute 'default nil
                        :family "Hack"
                        :height h-font-height
                        :weight 'normal))

;; Emoji font
(set-fontset-font t 'symbol (font-spec :family "Noto Emoji") (selected-frame) 'prepend)
;; I don't work on macOS much anymore.. but if i do:
;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") (selected-frame) 'prepend))

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

(defun export-compile-commands (project)
  "Generate a `compile_commands.json' file for a give project.
Select the appropriate cmake invocation via the `PROJECT' arg."
  ;; TODO: define project commands in a .dir-locals.el file.
  (interactive (list (read-from-minibuffer "project name: " nil nil nil nil)))
  (cond ((string= project "tes3mp")
         (let ((default-directory "~/src/openmw-tes3mp"))
           (shell-command
            "cmake -DDESIRED_QT_VERSION=5 -DRakNet_INCLUDES=/opt/morrowind/src/raknet/include -DLIBUNSHIELD_INCLUDE_DIR=/usr/include -DBullet_BulletCollision_LIBRARY=/usr/lib/libBulletCollision.so -DBullet_INCLUDE_DIR=/usr/include/bullet -DBullet_LinearMath_LIBRARY=/usr/lib/libLinearMath.so -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .")))
        ((string= project "openmw")
         (let ((default-directory "~/src/openmw"))
           (shell-command
            "cmake -DDESIRED_QT_VERSION=5 -DLIBUNSHIELD_INCLUDE_DIR=/usr/include -DBullet_BulletCollision_LIBRARY=/usr/lib/libBulletCollision.so -DBullet_INCLUDE_DIR=/usr/include/bullet -DBullet_LinearMath_LIBRARY=/usr/lib/libLinearMath.so -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .")))
        (t (shell-command "echo The given project name was not recognized."))))

(defun export-compile-commands-openmw ()
  "Shortcut for calling export-compile-commands for OpenMW."
  (interactive)
  (export-compile-commands "openmw"))

(defun export-compile-commands-tes3mp ()
  "Shortcut for calling export-compile-commands for TES3MP."
  (interactive)
  (export-compile-commands "tes3mp"))

;; Use lua-mode for PICO-8 source files
(setq auto-mode-alist (append '(("\\.p8$" . lua-mode))
                              auto-mode-alist))

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
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
;; Or make it smaller
(global-set-key (kbd "M--") 'text-scale-decrease)
(global-set-key (kbd "<f8>") 'text-scale-decrease)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
;; Toggle whitespace-mode
(global-set-key (kbd "C-c w") 'whitespace-mode)
;; Extra keybindings that make life great
(global-set-key (kbd "C-c r") 'rgrep)
(global-set-key (kbd "<f13>") 'rgrep) ;; Nice for macs
(global-set-key (kbd "C-x r b") 'revert-buffer)
(global-set-key (kbd "C-c q q q") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c d w") 'downcase-word)
(global-set-key (kbd "C-c u w") 'upcase-word)
(global-set-key (kbd "C-x u") 'upcase-region)
(global-set-key (kbd "C-x t m") 'menu-bar-mode)
(global-set-key (kbd "TAB") 'indent-appropriately)
(global-set-key (kbd "C-x C-v") 'clipboard-yank)
(global-set-key (kbd "C-^") 'enlarge-window)

;; Kill this buffer!
(substitute-key-definition 'kill-buffer 'kill-buffer-and-window global-map)

(setq-default org-support-shift-select t)

;;; Using Emacs as a window manager with this configuration
;; Inspired by this:
;; http://www.howardism.org/Technical/Emacs/new-window-manager.html
;; If you want to use Emacs as a window manager, all that's
;; needed is an ~/.xinitrc file with the following contents:
;;
;; exec /usr/bin/emacs
;;
;; The below functions are convenience wrappers for launching other GUI
;; programs while working in this environment, and the frame is set to
;; maximize to make this work (plus, I just prefer it that way.)

(defun launch-thing (thing)
  "Open 'THING', which should be some sort of X program."
  (start-process "" nil thing))

;; Maximize Emacs when it's opened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; How long did we take to load?
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "[STARTUP] Loading %s ... done (%.3fs)" load-file-name elapsed))

(provide 'init)
;;; init.el ends here
