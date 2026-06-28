
;; (use-package tree-sitter
;;   :straight t
;;   :hook
;;   (eglot . (lambda ()
;;                  (progn
;;                    (tree-sitter-hl-mode -1)
;;                    (message "IDK JAJA"))))
;;   ;;TODO: need to selectively enable this; it looks _TERRIBLE_ for YAML
;;   ;; (tree-sitter-after-on . (lambda ()
;;   ;;                           (tree-sitter-hl-mode -1)))
;;   ;; (lua-mode . tree-sitter-hl-mode)
;;   ;; (python-mode . tree-sitter-hl-mode)
;;   ;; (go-mode . (lambda () (tree-sitter-hl-mode 0)))
;;   )

;; (use-package tree-sitter-langs :straight t)

(when (treesit-available-p)
  (use-package treesit-auto
    :straight t
    :config (global-treesit-auto-mode)))
