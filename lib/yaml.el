;;; yaml.el --- Load yaml packages
;;; Commentary:
;; Packages related to yaml.
;;; Code:

;; The emacs major mode for editing files in the YAML data serialization format.
;; https://github.com/yoshiki/yaml-mode
;; TODO: don't always load company-ansible
(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (when (derived-mode-p 'yaml-mode)
                (add-to-list 'company-backends 'company-ansible)))))

;;; yaml.el ends here
