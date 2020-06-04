;;; lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:

;; Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :defer t
  :straight t
  :init
  (setq lsp-clients-python-library-directories (concat (getenv "HOME") ".local/lib/python3.8/site-packages")))

(use-package lsp-ui :straight t :defer t)

(use-package company-lsp
  :straight t
  :init
  (push 'company-lsp company-backends))

;;; lsp.el ends here
