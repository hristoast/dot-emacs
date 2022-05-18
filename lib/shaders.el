;;; shaders.el --- Load shaders packages
;;; Commentary:
;; Packages related to shaders.
;;; Code:

;; GLSL emacs mode
;; https://github.com/jimhourihan/glsl-mode
(use-package glsl-mode :defer t :straight t)

;; Open omwfx as GLSL
(add-to-list 'auto-mode-alist '("\\.omwfx$" . glsl-mode))

;;; shaders.el ends here
