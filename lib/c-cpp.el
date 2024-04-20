;;; c-cpp.el --- Load c/c++ packages
;;; Commentary:
;; Packages related to c/c++.
;;; Code:

(use-package cc-mode
  ;; :ensure-system-package (clang (bear . Bear))
  :straight nil
  :hook
  (before-save . clang-format-buffer)
  :init (setq c-basic-offset 4))

(use-package clang-format :defer t :straight t)

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :straight t)

;;; c-cpp.el ends here
