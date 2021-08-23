;;; c-cpp.el --- Load c/c++ packages
;;; Commentary:
;; Packages related to c/c++.
;;; Code:

;;TODO: document system packages that are required
;;TODO: bear, clangd

(use-package c++-mode :straight nil :init (setq c-basic-offset 4))

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :straight t)

;;; c-cpp.el ends here
