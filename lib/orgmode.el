;;; orgmode.el --- Load orgmode packages
;;; Commentary:
;; Packages related to orgmode.
;;; Code:

;; https://www.orgroam.com/
;;TODO: migrate to v2...
(use-package org-roam
  :straight (:type git :host github :repo "org-roam/org-roam-v1")
  :diminish org-roam-mode
  :config
  (setq-default
   org-roam-directory "~/src/org"))

(use-package org-contrib
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

;;TODO: for roam v2...
;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;;; orgmode.el ends here
