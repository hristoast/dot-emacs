;;; javascript.el --- Load javascript packages
;;; Commentary:
;; Packages related to javascript.
;;; Code:

(use-package javascript-mode
  :no-require t
  :init
  (add-hook 'js-mode-hook 'lsp-deferred))

;;; javascript.el ends here
