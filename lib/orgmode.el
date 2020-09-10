;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; https://www.orgroam.com/
(use-package org-roam
  :straight t
  :defer t
  :config
  (setq org-roam-directory "~/src/org"))

;; https://orgmode.org/elpa.html
(use-package org-plus-contrib
  :straight t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :defer t
  :init (setq-default
         org-startup-truncated nil
         org-support-shift-select t)
  (add-hook 'org-mode-hook 'org-roam-mode))

;;; orgmode.el ends here
