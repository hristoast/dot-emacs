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

;; https://github.com/golang/tools/blob/9abf76cc034c465c6e45ab8455a465543a8d552e/gopls/doc/emacs.md
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  ;; https://github.com/golang/tools/blob/d33eef8e6825f50394356b51ff2bbbe3d30e07e7/gopls/doc/user.md#installation
  ;; GO111MODULE=on go get golang.org/x/tools/gopls@latest
  ;; TODO: when i update use-package, do this:
  ;; :ensure-system-package
  ;; ("~/.local/go/bin/gopls" . "GOPATH=$HOME/.local/go go get golang.org/x/tools/gopls@latest")
  :straight t
  :init
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;;; golang.el ends here
