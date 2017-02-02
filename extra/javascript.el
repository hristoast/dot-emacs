;;; javascript.el -- for javascript functionality

;;; Commentary:
;; Require this from init.el to enable Javascript functionality.

;;; Code:
;; Company integration for tern (js)
;; https://github.com/proofit404/company-tern
;; (use-package company-tern :ensure t :defer t)

(use-package js2-highlight-vars :ensure t)

(use-package js2-mode
  :defer t
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (progn
                (tern-mode t)
                (add-to-list 'company-backends 'company-tern)))))

;; http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :ensure t
  :init
  (add-to-list 'exec-path
               (concat my-home "/.nvm/versions/node/v6.9.1/bin")))

(provide 'javascript)
;;; javascript.el ends here
