;;; terraform.el --- Load terraform packages
;;; Commentary:
;; Packages related to terraform.
;;
;;; Code:

;; Major mode of Terraform configuration file
;; https://github.com/syohex/emacs-terraform-mode
(use-package terraform-mode
  :straight t
  :defer t
  :hook
  (terraform-mode . terraform-format-on-save-mode))

;;; terraform.el ends here
