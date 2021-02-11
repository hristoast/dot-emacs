;;; css.el --- Load css packages
;;; Commentary:
;; Packages related to css.
;;; Code:

;; Built into Emacs
(use-package css-mode :straight t :hook (css-mode . skewer-css-mode))

;;; css.el ends here
