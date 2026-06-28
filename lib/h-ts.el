;;; h-ts.el --- eglot and related
;;; Commentary:
;; Packages related to eglot
;;; Code:

(when (treesit-available-p)
  (use-package treesit-auto
    :straight t
    :config
    ;; I really don't love the syntax highlighting changes
    ;; that treesit brings to Go..
    (setq treesit-auto-langs (remove 'go treesit-auto-langs))
    (global-treesit-auto-mode)))

;;; h-ts.el ends here
