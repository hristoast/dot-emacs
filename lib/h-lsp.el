;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:


;; Emacs client/library for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :straight t
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (gdscript-mode . lsp-deferred)
   (go-mode . (lambda ()
                (progn
                  (lsp-deferred)
                  (lsp-register-custom-settings
                   '(("gopls.completeUnimported" t t)
                     ("gopls.staticcheck" t t))))))
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (dockerfile-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (json-mode . lsp-deferred)
   (python-mode . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred)))
   (ruby-mode . lsp-deferred)
   (sh-mode . lsp-deferred)
   (yaml-mode . lsp-deferred))
  :init
  ;; Knob to disable the headerline breadcrum stuff
  (if (getenv "EMACS_LSP_NO_HEADERLINE_BREADCRUMB")
      (lsp-headerline-breadcrumb-mode 0))
  (setq
   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
   gc-cons-threshold (* 100 1024 1024)
   lsp-idle-delay 0.1
   read-process-output-max (* 1024 1024)
   lsp-prefer-flymake nil))

;; UI integrations for lsp-mode
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui :straight t)

;;; h-lsp.el ends here
