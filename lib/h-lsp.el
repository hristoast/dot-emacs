;;; h-lsp.el --- lsp-mode and related
;;; Commentary:
;; Packages related to lsp-mode
;;; Code:

(if (getenv "EMACS_LSP_MODE")
    (progn
      ;; Emacs client/library for the Language Server Protocol
      ;; https://github.com/emacs-lsp/lsp-mode
      (use-package lsp-mode
        :straight t
        :hook ((gdscript-mode . lsp-deferred)
               (go-mode .
                        (lsp-deferred (lambda ()
                                        (lsp-register-custom-settings
                                         '(("gopls.completeUnimported" t t)
                                           ("gopls.staticcheck" t t))))))
               (c-mode . lsp-deferred)
               (c++-mode . lsp-deferred)
               (js-mode . lsp-deferred)
               (python-mode . lsp-deferred)
               (sh-mode . lsp-deferred))
        :init
        (setq
         lsp-clients-python-library-directories
         (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
        :config
        ;; Disable python "features" that are useless to me
        (setq lsp-pyls-plugins-mccabe-enabled nil
              lsp-pyls-plugins-pycodestyle-enabled nil))

      (use-package lsp-ui :straight t))

  ;; Emacs Polyglot: an Emacs LSP client that stays out of your way:
  ;; https://github.com/joaotavora/eglot
  (use-package eglot :straight t
    ;; Hook into modes that I always want LSP functionality in.
    :hook ((gdscript-mode . eglot-ensure)
           (go-mode . eglot-ensure)
           (c-mode . eglot-ensure)
           (c++-mode . eglot-ensure)
           (js-mode . eglot-ensure)
           (python-mode . eglot-ensure)
           (sh-mode . eglot-ensure))
    :init
    (setq
     lsp-clients-python-library-directories
     (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
    :config
    ;; Disable python "features" that are useless to me
    (setq lsp-pyls-plugins-mccabe-enabled nil
          lsp-pyls-plugins-pycodestyle-enabled nil)))

;;; h-lsp.el ends here
