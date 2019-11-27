;;; css.el --- Load css packages
;;; Commentary:
;; Packages related to css.
;;; Code:

;; Built into Emacs
(use-package css-mode
  :ensure t
  :init (add-hook 'css-mode-hook 'skewer-css-mode))

;;; css.el ends here
