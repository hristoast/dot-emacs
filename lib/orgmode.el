;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; https://www.orgroam.com/
(use-package org-roam
  :straight t
  :defer t
  :diminish org-roam-mode
  :config
  (setq-default org-roam-directory "~/src/org"))

;; https://orgmode.org/elpa.html
(use-package org-plus-contrib
  :straight t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :defer t
  :hook (org-mode . org-roam-mode)
  :init
  (setq-default
   org-log-done t
   org-startup-folded nil
   org-startup-truncated nil
   org-support-shift-select t))

;;; orgmode.el ends here
