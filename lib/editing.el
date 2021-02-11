;;; editing.el --- Packages related to editing.
;;; Commentary:
;; Packages related to editing.
;;; Code:

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(use-package autorevert :diminish auto-revert-mode :straight t)

;; Clean auto-indent and backspace unindent
;; https://github.com/pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :straight t
  :config
  (electric-indent-mode -1)
  (setq clean-aindent-is-simple-indent t))

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :straight t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-echo-delay 0
   company-idle-delay 0.2
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20)
  ;; Default colors are awful - borrowed these from gocode (thanks!):
  ;; https://github.com/nsf/gocode/tree/master/emacs-company#color-customization
  ;; See: M-x list-colors-display
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Colors.html
  (set-face-attribute
   'company-preview nil :foreground "black" :underline t)
  (set-face-attribute
   'company-preview-common nil :inherit 'company-preview)
  (set-face-attribute
   'company-tooltip nil :background "gray44" :foreground "black")
  (set-face-attribute
   'company-tooltip-selection nil :background "LavenderBlush4" :foreground "DarkOliveGreen2")
  (set-face-attribute
   'company-tooltip-common nil :foreground "OliveDrab3" :weight 'bold)
  (set-face-attribute
   'company-tooltip-common-selection nil :foreground "black" :weight 'bold))

;; Various completion functions using Ivy
;; https://github.com/abo-abo/swiper / https://melpa.org/#/counsel
(use-package counsel :defer t :straight t)

(unless (getenv "EMACS_NO_DIMMER")
  ;; Interactively highlight which buffer is active by dimming the others.
  ;; https://github.com/gonewest818/dimmer.el
  (use-package dimmer
    :straight t
    :config
    (setq
     dimmer-fraction 0.25
     dimmer-watch-frame-focus-events nil)
    (dimmer-mode t)))

;; A minor mode that guesses the indentation offset originally used for
;; creating source code files and transparently adjusts the corresponding
;; settings in Emacs, making it more convenient to edit foreign files
;; https://github.com/jscheid/dtrt-indent
;; TODO: remove this, perhaps?
(use-package dtrt-indent
  :straight t
  :config
  (setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1))

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :straight t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  ;; https://www.flycheck.org/en/latest/user/installation.html#use-package
  :init (global-flycheck-mode))

;; Ignore "vendor" directories when rgrepping. Useful for some go projects I've worked on.
(use-package grep
  :straight nil
  :config
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories "vendor"))

(unless (getenv "EMACS_NO_HL_TODO")
  ;; Highlight TODO keywords
  ;; https://github.com/tarsius/hl-todo
  (use-package hl-todo
    :straight t
    :config
    (setq hl-todo-keyword-faces
      '(("TODO"   . "#ffff00")))
    (global-hl-todo-mode t)))

;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
(use-package ido :config (ido-mode t) :straight t)

;; marginalia.el - Marginalia in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia :straight t :init (marginalia-mode))

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Rainbow mode - #000 #fff #f00 #ff0 #00f #0f0 #800080 #00ffff #ff00ff
;; https://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

;; Provides language-aware editing commands based on source code parsers.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
(use-package semantic
  :straight t
  :commands semantic-mode
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; Minor mode for Emacs that deals with parens
;; pairs and tries to be smart about it
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :straight t
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
(use-package swiper :straight t)

;; undo-tree.el --- Treat undo history as a tree
;; http://www.dr-qubit.org/undo-tree/undo-tree.el
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :config
  (setq
   ;; TODO: make a special dir for these.
   ;; undo-tree-auto-save-history t
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; windmove, built into Emacs: http://is.gd/63r6U0
(use-package windmove
  :straight t
  :bind
  ("M-e" . windmove-left)
  ("M-u" . windmove-right)
  ("M-k" . windmove-up)
  ("M-j" . windmove-down))

;; Unobtrusively trim extraneous whitespace *ONLY* in lines edited
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :straight t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; Yet another snippet extension
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  ;; Shut yas up! Disables startup noise
  (setq-default yas-verbosity 0)
  :config
  (yas-global-mode 1))

;; zygospore: Reversible C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(use-package zygospore
  :straight t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;;; editing.el ends here
