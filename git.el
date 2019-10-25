;;; git.el --- Load git packages
;;; Commentary:
;; Packages related to git.
;;; Code:

;; Emacs mode for .gitignore files
;; https://github.com/magit/git-modes/blob/master/gitignore-mode.el
(use-package gitignore-mode :defer t)

;; A Git Porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :bind
  ("C-c g d" . magit-diff-range)
  ("C-x g" . magit-status)
  :config
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (setf (alist-get 'stashes magit-section-initial-visibility-alist) 'show))

;;; git.el ends here
