;;; h-extras.el --- Load "extras" packages
;;; Commentary:
;; I consider "extras" to be kind of high-level, but it's
;; anything not related to a specific language or file format.
;;; Code:

;; TODO: https://github.com/pashky/restclient.el
;; TODO: https://github.com/rnkn/fountain-mode

;; An Emacs web feeds client
;; https://github.com/skeeto/elfeed
(use-package elfeed :straight t :defer t)

;; A frontend for elfeed, similar to mu4e landing page. The UI can be customized with org mode.
;; https://github.com/Manoj321/elfeed-dashboard
(use-package elfeed-dashboard
  :straight t
  :bind ("C-x w" . elfeed-dashboard)
  :config
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links)
  :init
  (let ((h/elfeed-dashboard-file (or (getenv "EMACS_ELFEED_DASHBOARD_FILE")
                                     "~/src/org/elfeed-dashboard.org")))
    (setq elfeed-dashboard-file h/elfeed-dashboard-file)
    (unless (getenv "EMACS_ELFEED_DASHBOARD_NO_DL")
      (when (not (file-exists-p h/elfeed-dashboard-file))
        (shell-command
         (concat
          "curl https://raw.githubusercontent.com/Manoj321/elfeed-dashboard/main/elfeed-dashboard.org -o "
          h/elfeed-dashboard-file))))))

;; Configure the Elfeed RSS reader with an Orgmode file
;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (or (getenv "EMACS_ELFEED_ORG_FILE")
                                       "~/src/org/elfeed.org"))))

;;; h-extras.el ends here
