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
  (defun hristoast-golang-before-save ()
    "Do the right thing whether it's lsp-mode or eglot."
        (progn
          (lsp-format-buffer)
          (lsp-organize-imports)))
  :hook
  (before-save . hristoast-golang-before-save))

;;; golang.el ends here
