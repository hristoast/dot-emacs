;;; c-cpp.el --- Load c/c++ packages
;;; Commentary:
;; Packages related to c/c++.
;;; Code:

;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C,
;; Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;; (use-package cc-mode
;;   :straight t
;;   :init
;;   (defun generate-compile-commands-json ()
;;     ""
;;     (interactive)))
;;   :hook (before-save . clang-format-save-hook)
;;   :init
;;   (defun clang-format-save-hook ()
;;     "Run clang-format on save when in c or c++ mode when the variable
;;      'clang-forma-on-save' is set. Put the following into a .dir-locals.el file
;;      in your project to use this:

;;      ((nil . ((eval . (setq clang-format-on-save t)))))"
;;     (interactive)
;;     (when (and (boundp 'clang-format-on-save)
;;                (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)))
;;       (clang-format-buffer))))

;; Clang format
;; http://clang.llvm.org/docs/ClangFormat.html
;; http://clang.llvm.org/docs/ClangFormatStyleOptions.html
(use-package clang-format :straight t :defer t)

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :straight t)

;; ;; Shows an inline arguments hint for the C/C++ function at point
;; ;; https://github.com/abo-abo/function-args
(use-package function-args
  :straight t
  :config
  (fa-config-default)
  :bind
  (:map c-mode-map
        ("M-o" . fa-show))
  (:map c++-mode-map
        ("M-o" . fa-show)))

;;; c-cpp.el ends here
