;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; https://orgmode.org/elpa.html
(use-package org-plus-contrib
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :defer t
  :init (setq-default org-startup-truncated nil))

;;; orgmode.el ends here
