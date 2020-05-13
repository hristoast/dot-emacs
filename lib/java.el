;;; java.el --- Load java packages
;;; Commentary:
;; Packages related to java.
;;; Code:

;; A Better Java Development Environment for Emacs
;; https://github.com/mopemope/meghanada-emacs
(use-package meghanada
  :straight t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-default meghanada-mode t)
              (meghanada-server-start)
              ;; TODO: 10 below is arbitrary, the server takes
              ;; around 6 seconds to start on my primary workstation
              (run-with-timer 10 nil 'meghanada-client-direct-connect))))

;;; java.el ends here
