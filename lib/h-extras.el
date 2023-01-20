;;; h-extras.el --- Load "extras" packages
;;; Commentary:
;; I consider "extras" to be kind of high-level, but it's
;; anything not related to a specific language or file format.
;;; Code:

;; TODO: https://github.com/pashky/restclient.el
;; TODO: https://github.com/rnkn/fountain-mode
;; TODO: https://github.com/skuro/plantuml-mode
;; TODO: https://github.com/emacs-eaf/eaf-browser

;; A d2 extension for Emacs. This was heavily inspired of Mermaid Mode.
;; https://github.com/andorsk/d2-mode
;; Requires: https://github.com/terrastruct/d2
(use-package d2-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.d2$" . d2-mode)))

;; A guitar fretboard visualization in Emacs.
;; https://github.com/vifon/fretboard.el#fretboardel
(use-package fretboard
  :straight (:host github :repo "vifon/fretboard.el")
  :defer t)

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
  (let ((hristoast-elfeed-dashboard-file (or (getenv "EMACS_ELFEED_DASHBOARD_FILE")
                                     "~/src/org/elfeed-dashboard.org")))
    (setq elfeed-dashboard-file hristoast-elfeed-dashboard-file)
    (unless (getenv "EMACS_ELFEED_DASHBOARD_NO_DL")
      (when (not (file-exists-p hristoast-elfeed-dashboard-file))
        (shell-command
         (concat
          "curl https://raw.githubusercontent.com/Manoj321/elfeed-dashboard/main/elfeed-dashboard.org -o "
          hristoast-elfeed-dashboard-file))))))

;; Configure the Elfeed RSS reader with an Orgmode file
;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (or (getenv "EMACS_ELFEED_ORG_FILE")
                                       "~/src/org/elfeed.org"))))

;; One-frame-per-action GIF recording for optimal quality/size ratio
;; https://gitlab.com/ambrevar/emacs-gif-screencast
(use-package gif-screencast
  :straight t
  :bind (("ESC g r" . gif-screencast-start-or-stop)
         ("ESC g p" . gif-screencast-pause)
         ("ESC g s" . gif-screencast-stop)))

;; Imbue Emacs with power!
;; https://github.com/elizagamedev/power-mode.el#power-modeel
(use-package power-mode
  :straight (power-mode
               :type git
               :host github
               :repo "elizagamedev/power-mode.el"))

;; A GNU Emacs major mode that acts as a front end to mpc.
;; https://github.com/jorenvo/simple-mpc
(unless (getenv "EMACS_NO_MPD")
  (use-package simple-mpc
    ;; :ensure-system-package (mpc mpd)
    :straight t
    :bind ("C-c s m" . simple-mpc)))

;;; h-extras.el ends here
