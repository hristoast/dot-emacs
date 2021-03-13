;;; terraform.el --- Load terraform packages
;;; Commentary:
;; Packages related to terraform.
;;
;; NOTE: I don't use these very often, so they may or may not be up to date.
;;
;;; Code:

;; https://github.com/rafalcieslak/emacs-company-terraform
;; Company backend for terraform files
(use-package company-terraform :defer t :straight t)

;; Major mode of Terraform configuration file
;; https://github.com/syohex/emacs-terraform-mode
(use-package terraform-mode
  :straight t
  :init (company-terraform-init))

;;; terraform.el ends here
