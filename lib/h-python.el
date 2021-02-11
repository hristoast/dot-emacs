;;; h-python.el --- Load python packages
;;; Commentary:
;; Packages related to python.
;;; Code:

;; Python Black for Emacs
;; https://github.com/proofit404/blacken
(use-package blacken :defer t :straight t)

;; Built into Emacs
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python-mode :hook (python-mode . blacken-mode))

;;; h-python.el ends here
