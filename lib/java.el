;;; java.el --- Load java packages
;;; Commentary:
;; Packages related to java.
;;
;; NOTE: I don't use this very often, so it may or may not be up to date.
;;
;;; Code:

;; A Better Java Development Environment for Emacs
;; https://github.com/mopemope/meghanada-emacs
;; (use-package meghanada
;;   :straight t
;;   :hook (java-mode . (lambda ()
;;                        (setq-default meghanada-mode t)
;;                        (meghanada-server-start)
;;                        ;; TODO: 10 below is arbitrary, the server takes
;;                        ;; around 6 seconds to start on my primary workstation
;;                        (run-with-timer 10 nil 'meghanada-client-direct-connect))))

;;; java.el ends here
