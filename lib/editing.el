;;; editing.el --- Packages related to editing.
;;; Commentary:
;; Packages related to editing.
;;; Code:

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Autorevert.html
(use-package autorevert :diminish auto-revert-mode :ensure t)

;; Clean auto-indent and backspace unindent
;; https://github.com/pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :ensure t
  :config
  (electric-indent-mode -1)
  (setq clean-aindent-is-simple-indent t))

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :ensure t
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

;; Various completion functions using Ivy
;; https://github.com/abo-abo/swiper / https://melpa.org/#/counsel
(use-package counsel :defer t :ensure t)

;; A minor mode that guesses the indentation offset originally used for
;; creating source code files and transparently adjusts the corresponding
;; settings in Emacs, making it more convenient to edit foreign files
;; https://github.com/jscheid/dtrt-indent
;; TODO: remove this, perhaps?
(use-package dtrt-indent
  :ensure t
  :config
  (setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1))

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :ensure t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Interactively Do Things
;; http://emacswiki.org/emacs/InteractivelyDoThings
(use-package ido :config (ido-mode t) :ensure t)

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rainbow mode - #000 #fff #f00 #ff0 #00f #0f0 #800080 #00ffff #ff00ff
;; https://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Provides language-aware editing commands based on source code parsers.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Semantic.html
;; (require 'semantic)
(use-package semantic
  :ensure t
  :commands semantic-mode
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

;; Minor mode for Emacs that deals with parens
;; pairs and tries to be smart about it
;; https://github.com/Fuco1/smartparens
(use-package smartparens
  :ensure t
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
(use-package swiper :ensure t)

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

;; windmove, built into Emacs: http://is.gd/63r6U0
(use-package windmove
  :ensure t
  :bind
  ("M-e" . windmove-left)
  ("M-u" . windmove-right)
  ("M-k" . windmove-up)
  ("M-j" . windmove-down))

;; Unobtrusively trim extraneous whitespace *ONLY* in lines edited
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

;; Yet another snippet extension
;; http://capitaomorte.github.io/yasnippet/
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  ;; Shut yas up! Disables startup noise
  (setq-default yas-verbosity 0)
  :config
  (yas-global-mode 1))

;; zygospore: Reversible C-x 1
;; https://github.com/LouisKottmann/zygospore.el
(use-package zygospore
  :ensure t
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

;;; editing.el ends here
