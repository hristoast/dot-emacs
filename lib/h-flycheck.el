;;; h-flycheck.el --- Packages related to flycheck.
;;; Commentary:
;; Packages related to flycheck.
;;; Code:

;; Syntax checking for GNU Emacs - http://www.flycheck.org/
(use-package flycheck
  :straight t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previous-error))
  ;; https://www.flycheck.org/en/latest/user/installation.html#use-package
  :init (global-flycheck-mode))

;;; h-flycheck.el ends here
