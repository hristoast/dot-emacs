;;; git.el --- Load git packages
;;; Commentary:
;; Packages related to git.
;;; Code:

;; https://github.com/Wilfred/difftastic
;; https://github.com/pkryger/difftastic.el
(use-package difftastic
  :defer t
  :straight
  (difftastic :type git
              :host github
              :repo "pkryger/difftastic.el")
  :hook (magit-mode . difftastic-bindings-mode))

;; Emacs mode for .gitignore files
;; https://github.com/magit/git-modes/blob/master/gitignore-mode.el
(use-package git-modes :defer t :straight t)

;; A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  ;; :ensure-system-package git
  :straight t
  :bind
  ("C-c g d" . magit-diff-range)
  ("C-x g" . magit-status)
  :config
  (unless (getenv "EMACS_MAGIT_HIDE_UNPUSHED")
    (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show))

  (unless (getenv "EMACS_MAGIT_HIDE_UNTRACKED")
    (setf (alist-get 'untracked magit-section-initial-visibility-alist) 'show))

  (unless (getenv "EMACS_MAGIT_HIDE_STASHES")
    (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show)))

;; Needed for Forge
(use-package cond-let
  :straight (:host github :repo "tarsius/cond-let"))

;; https://github.com/magit/forge
(use-package forge :straight t :after magit)

;;; git.el ends here
