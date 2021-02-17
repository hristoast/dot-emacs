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
               (go-mode . (lambda ()
                            (progn
                              (lsp-deferred)
                              (lsp-register-custom-settings
                               '(("gopls.completeUnimported" t t)
                                 ("gopls.staticcheck" t t)
                                 ("gopls.codelenses" nil nil))))))
               (c-mode . lsp-deferred)
               (c++-mode . lsp-deferred)
               (js-mode . lsp-deferred)
               (python-mode . lsp-deferred)
               (ruby-mode . lsp-deferred)
               (sh-mode . lsp-deferred))
        :init
        ;; Knob to disable the headerline breadcrum stuff
        (if (getenv "EMACS_LSP_NO_HEADERLINE_BREADCRUMB")
            (lsp-headerline-breadcrumb-mode 0))
        (setq
         lsp-prefer-flymake nil
         lsp-clients-python-library-directories
         (concat (getenv "HOME") ".local/lib/python3.8/site-packages"))
        :config
        ;; Disable python "features" that are useless to me
        (setq lsp-pyls-plugins-mccabe-enabled nil
              lsp-pyls-plugins-pycodestyle-enabled nil))

      ;; UI integrations for lsp-mode
      ;; https://emacs-lsp.github.io/lsp-ui/
      (use-package lsp-ui :straight t))

  ;; Emacs Polyglot: an Emacs LSP client that stays out of your way:
  ;; https://github.com/joaotavora/eglot
  (use-package eglot :straight t
    ;; Hook into modes that I always want LSP functionality in.
    :hook
    ((gdscript-mode . eglot-ensure)
     (go-mode . eglot-ensure)
     (c-mode . eglot-ensure)
     (c++-mode . eglot-ensure)
     (js-mode . eglot-ensure)
     (python-mode . eglot-ensure)
     (ruby-mode . eglot-ensure)
     (sh-mode . eglot-ensure))
    :init
    (setq eglot-workspace-configuration
          '((pyls
             (plugins
              (mccabe
               (enabled . nil))
              (pycodestyle
               (enabled . nil))
              (pydocstyle
               (enabled . t))
              (jedi_completion
               (fuzzy . t)
               (follow_builtin_imports . :json-false))))))))

;;; h-lsp.el ends here
