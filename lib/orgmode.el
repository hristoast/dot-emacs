;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; Needed for org-roam
(use-package emacsql :straight t)

;; https://www.orgroam.com/
(use-package org-roam
  :straight t
  :diminish org-roam-mode
  :config
  (setq-default
   org-roam-directory "~/src/org")
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;; orgmode.el ends here
