;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:


;; Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :straight t
  :hook
  ((gdscript-mode . lsp-deferred)
   (go-mode . (lambda ()
                (progn
                  (lsp-deferred)
                  (lsp-register-custom-settings
                   '(("gopls.completeUnimported" t t)
                     ("gopls.staticcheck" t t))))))
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (web-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (json-mode . lsp-deferred)
   (python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)))
   (ruby-mode . lsp-deferred)
   (sh-mode . lsp-deferred))
  :init
  ;; Knob to disable the headerline breadcrum stuff
  (if (getenv "EMACS_LSP_NO_HEADERLINE_BREADCRUMB")
      (lsp-headerline-breadcrumb-mode 0))
  (setq
   lsp-prefer-flymake nil
   lsp-clients-python-library-directories
   ;;TODO: Dehardcode this path
   (concat (getenv "HOME") ".local/lib/python3.9/site-packages"))
  :config
  ;; Disable python "features" that are useless to me
  (setq lsp-pyls-plugins-mccabe-enabled nil
        lsp-pyls-plugins-pycodestyle-enabled nil))

;; UI integrations for lsp-mode
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui :straight t)

;;; h-lsp.el ends here
