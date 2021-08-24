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

(unless (getenv "EMACS_NO_HL_TODO")
  ;; Highlight TODO keywords
  ;; https://github.com/tarsius/hl-todo
  (use-package hl-todo
    :straight t
    :config
    (setq hl-todo-keyword-faces
      '(("FIXME"   . "#ffff00")
        ("TODO"   . "#ffff00")
        ("XXX"   . "#ffff00")))
    (global-hl-todo-mode t)))

;; marginalia.el - Marginalia in the minibuffer
;; https://github.com/minad/marginalia
(use-package marginalia :straight t :init (marginalia-mode))

;; Enable code-folding for prog-mode modes
(use-package hideshow
  :straight nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))

;; Emacs rainbow delimiters mode
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; TODO: https://github.com/karthink/popper

;; Rainbow mode - #000 #fff #f00 #ff0 #00f #0f0 #800080 #00ffff #ff00ff
;; https://julien.danjou.info/projects/emacs-packages
(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

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
  (sp-with-modes sp-lisp-modes
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
                                    (t (not (sp-point-in-string-or-comment)))))))
  ;; Don't pair { in web-mode
  (sp-with-modes 'web-mode
    (sp-local-pair "\{" nil :actions nil)))

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

;; Emacs package that displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key/
(use-package which-key
  :diminish which-key-mode
  :straight t
  :config (which-key-mode))

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
