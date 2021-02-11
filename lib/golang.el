;;; golang.el --- Load golang packages
;;; Commentary:
;; Packages related to golang.
;;; Code:

(unless (getenv "EMACS_NO_GOPATH") ;; Disable setting GOPATH in this config.
  ;; Set up so that go get doesn't spam $HOME
  (setenv "GOPATH" (concat (getenv "HOME") "/.local/go")))

(unless (getenv "EMACS_NO_GOBINPATH") ;; Don't try to add a custom bin dir to `exec-path'.
  ;; Add the resulting bindir to the exec-path
  (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))

(use-package go-mode
  :straight t
  :init
  (defun h/golang-before-save ()
    "Do the right thing whether it's lsp-mode or eglot."
    (if (getenv "EMACS_LSP_MODE")
        ;; lsp-mode
        (progn
          (lsp-format-buffer)
          (lsp-organize-imports))

      ;; eglot
      (progn
        (call-interactively 'eglot-format-buffer)
        (call-interactively 'eglot-code-action-organize-imports))))
  :hook (before-save . h/golang-before-save))

;;; golang.el ends here
