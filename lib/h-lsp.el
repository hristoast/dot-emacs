;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:

(if (getenv "EMACS_LSP_MODE")
    ;; Emacs client/library for the Language Server Protocol
    ;; https://github.com/emacs-lsp/lsp-mode
    (use-package lsp-mode
      :straight t
      :init
      (setq
       lsp-clients-python-library-directories
       (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
      :config
      ;; Disable python "features" that are useless to me
      (setq lsp-pyls-plugins-mccabe-enabled nil
            lsp-pyls-plugins-pycodestyle-enabled nil))

  ;; Emacs Polyglot: an Emacs LSP client that stays out of your way:
  ;; https://github.com/joaotavora/eglot
  (use-package eglot :straight t
    :init
    (setq
     lsp-clients-python-library-directories
     (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
    :config
    ;; Disable python "features" that are useless to me
    (setq lsp-pyls-plugins-mccabe-enabled nil
          lsp-pyls-plugins-pycodestyle-enabled nil)))

(use-package lsp-ui :straight t)

;;; h-lsp.el ends here
