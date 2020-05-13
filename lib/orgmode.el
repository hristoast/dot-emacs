;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; https://orgmode.org/elpa.html
(use-package org-plus-contrib
  :straight t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :defer t
  :init (setq-default
         org-startup-truncated nil
         org-support-shift-select t))

;;; orgmode.el ends here
