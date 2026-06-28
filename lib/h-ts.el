;;; h-ts.el --- eglot and related
;;; Commentary:
;; Packages related to eglot
;;; Code:

(when (treesit-available-p)
  (use-package treesit-auto
    :straight t
    :config (global-treesit-auto-mode)))

;;; h-ts.el ends here
