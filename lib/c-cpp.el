;;; c-cpp.el --- Load c/c++ packages
;;; Commentary:
;; Packages related to c/c++.
;;; Code:

;;TODO: document system packages that are required
;;TODO: bear, clangd

;; Clang format
;; http://clang.llvm.org/docs/ClangFormat.html
;; http://clang.llvm.org/docs/ClangFormatStyleOptions.html
(use-package clang-format :straight t :defer t)

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :straight t)

;;; c-cpp.el ends here
