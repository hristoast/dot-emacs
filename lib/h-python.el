;;; h-python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken
  ;; :ensure-system-package black
  :straight t
  :hook
  (python-mode . blacken-mode))

;;; h-python.el ends here
