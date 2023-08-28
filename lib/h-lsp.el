;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:

(if (getenv "EMACS_USE_LSP_MODE")
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
       (ansible-mode . lsp-deferred)
       (c-mode . lsp-deferred)
       (c++-mode . lsp-deferred)
       (dockerfile-mode . lsp-deferred)
       ;; (web-mode . lsp-deferred)
       (js-mode . lsp-deferred)
       (json-mode . lsp-deferred)
       (lua-mode . lsp-deferred)
       (nginx-mode . lsp-deferred)
       (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred)))
       (ruby-mode . lsp-deferred)
       (sh-mode . lsp-deferred)
       (toml-mode . lsp-deferred)
       (yaml-mode . lsp-deferred))
      :init
      ;; Knob to ensable the headerline breadcrum stuff
      (lsp-headerline-breadcrumb-mode -1)
      (if (not (getenv "EMACS_LSP_HEADERLINE_BREADCRUMB"))
          (setq lsp-headerline-breadcrumb-enable nil))

      (setq
       ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
       gc-cons-threshold (* 100 1024 1024)
       lsp-idle-delay 0.1
       read-process-output-max (* 1024 1024)
       lsp-prefer-flymake nil))

  ;; UI integrations for lsp-mode
  ;; https://emacs-lsp.github.io/lsp-ui/
  (use-package lsp-ui :straight t)

  ;; Default to Eglot
  (use-package eglot
    :straight nil
    :defer t
    :hook
    ((ansible-mode . eglot-ensure)
     (c-mode . eglot-ensure)
     (c++-mode . eglot-ensure)
     (dockerfile-mode . eglot-ensure)
     (go-mode . eglot-ensure)
     (js-mode . eglot-ensure)
     (json-mode . eglot-ensure)
     (lua-mode . eglot-ensure)
     (nginx-mode . eglot-ensure)
     (python-mode . eglot-ensure)
     (ruby-mode . eglot-ensure)
     (sh-mode . eglot-ensure)
     (toml-mode . eglot-ensure)
     (yaml-mode . eglot-ensure))))

;;; h-lsp.el ends here
