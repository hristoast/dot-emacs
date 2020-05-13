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

(unless (getenv "EMACS_NO_GOCODE") ;; Don't try to set up `gocode'
  ;; company-mode autocompletion for golang
  ;; https://github.com/nsf/gocode/tree/master/emacs-company
  (use-package company-go :defer t :straight t))

(use-package go-mode
  :straight t
  :init
  (unless (getenv "EMACS_NO_GOCODE") ;; Don't try to set up `gocode'
    (add-hook 'go-mode-hook (lambda () (add-to-list 'company-backends 'company-go))))
  (setq-default gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-eldoc :defer t :straight t)

;;; golang.el ends here
