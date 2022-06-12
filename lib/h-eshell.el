;;; h-eshell.el --- Eshell stuff
;;; Commentary:
;; Stuff to customize eshell and make it cooler.
;;; Code:

(use-package eshell
  :straight nil
  :config
  ;; Aliases
  (defun eshell/.. ()
    (eshell/cd ".."))

  (defun eshell/... ()
    (eshell/cd "../.."))

  (defun eshell/.... ()
    (eshell/cd "../../.."))

  (defun eshell/..... ()
    (eshell/cd "../../../.."))

  (defun eshell/ll (&rest args)
    (eshell/ls "-lh" args))

  (defun eshell/la (&rest args)
    (eshell/ls "-lah" args))

  (defun eshell/lA (&rest args)
    (eshell/ls "-lAh" args))

  (defun eshell/XXX ()
    (eshell/exit))

  ;; Settings
  (setq eshell-history-size 2048)

  ;; Colors
  (custom-set-faces
   '(eshell-prompt
     ((t (:foreground "LimeGreen" :weight bold))))))

;; syntax highlighting for Eshell
;; https://github.com/akreisher/eshell-syntax-highlighting/
(unless (getenv "EMACS_NO_ESHELL_SYNTAX")
  (use-package eshell-syntax-highlighting
    :after esh-mode
    :straight t
    :config (eshell-syntax-highlighting-global-mode)))

;; Display some system information when launching Eshell
;; https://github.com/Phundrak/eshell-info-banner.el
(unless (getenv "EMACS_NO_ESHELL_BANNER")
  (use-package eshell-info-banner
    :defer t
    :straight
    (eshell-info-banner :type git
                        :host github
                        :repo "phundrak/eshell-info-banner.el")
    :hook (eshell-banner-load . eshell-info-banner-update-banner)))

;;; h-eshell.el ends here
