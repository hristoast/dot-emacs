;;; h-system.el --- Stuff related to the system.
;;; Commentary:
;; Stuff related to the system, and things that should load early.
;;; Code:

;; Diminished modes are minor modes with no modeline display
;; http://www.eskimo.com/~seldon/diminish.el
;; Load this early on.
(use-package diminish :straight t)

;; TODO: This causes a hang on my system, figure out why..
;; https://github.com/jwiegley/use-package/tree/a7422fb8ab1baee19adb2717b5b47b9c3812a84c#use-package-ensure-system-package
;; (use-package use-package-ensure-system-package :straight t)

;;; h-system.el ends here
