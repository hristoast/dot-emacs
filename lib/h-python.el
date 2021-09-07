;;; h-python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken
  ;; :ensure-system-package black
  :straight t
  :hook
  (python-mode . blacken-mode))

;; lsp-mode client leveraging Pyright language server
;; https://github.com/emacs-lsp/lsp-pyright
;; https://github.com/microsoft/pyright
;;TODO: disable "cyclomatic complexity" checks, they aren't useful
(use-package lsp-pyright
  ;; :ensure-system-package (pylsp . "python3 -m pip install python-lsp-server")
  :straight t
  :defer t)

;;; h-python.el ends here
