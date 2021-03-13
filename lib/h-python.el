;;; h-python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken
  :straight t
  :hook
  (python-mode . blacken-mode))

;; lsp-mode client leveraging Pyright language server
;; https://github.com/emacs-lsp/lsp-pyright
;; https://github.com/microsoft/pyright
(use-package lsp-pyright :straight t :defer t)

;;; h-python.el ends here
