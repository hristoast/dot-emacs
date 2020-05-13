;;; javascript.el --- Load javascript packages
;;; Commentary:
;; Packages related to javascript.
;;; Code:

;; Tern backend for company-mode.
;; https://github.com/proofit404/company-tern
(use-package company-tern :defer t :straight t)

;; Tern: Intelligent JavaScript tooling
;; http://ternjs.net/doc/manual.html#emacs
;; TODO: require `tern' installation.
(use-package tern
  :straight t
  :commands tern-mode
  :init
  (add-hook 'js-mode-hook
            (lambda ()
              (progn
                (add-to-list 'exec-path (concat (getenv "HOME") "/.local/bin"))
                (add-to-list 'company-backends 'company-tern)
                (tern-mode t)))))

;;; javascript.el ends here
