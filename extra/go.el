;;; go.el -- for Golang functionality

;;; Commentary:
;; Require this from init.el to enable Golang functionality.

;;; Code:
;; company-mode autocompletion for golang
;; https://github.com/nsf/gocode/tree/master/emacs-company
(use-package company-go
  :ensure t
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  ;; Set up environment variables so Flycheck can find gocode.
  (setenv "GOPATH" (concat my-src "/golibs"))
  (add-to-list 'exec-path my-bin))

(use-package go-mode :defer t :ensure t)

(provide 'go)
;;; go.el ends here
