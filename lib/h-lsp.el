;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:

;; Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :defer t
  :straight t
  :init
  (setq
   lsp-clients-python-library-directories
   (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
  :config
  ;; Disable python "features" that are useless to me
  (setq lsp-pyls-plugins-mccabe-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil))

(use-package lsp-ui :straight t :defer t)

(use-package company-lsp
  :straight t
  :init
  (setq company-lsp-enable-recompletion t)
  (push 'company-lsp company-backends))

;;; lsp.el ends here
