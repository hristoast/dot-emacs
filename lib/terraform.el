;;; terraform.el --- Load terraform packages
;;; Commentary:
;; Packages related to terraform.
;;; Code:

;; https://github.com/rafalcieslak/emacs-company-terraform
;; Company backend for terraform files
(use-package company-terraform :defer t :ensure t)

;; Major mode of Terraform configuration file
;; https://github.com/syohex/emacs-terraform-mode
(use-package terraform-mode
  :ensure t
  :init (company-terraform-init))

;;; terraform.el ends here
