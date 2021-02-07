;;; c-cpp.el --- Load c/c++ packages
;;; Commentary:
;; Packages related to c/c++.
;;; Code:

;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C,
;; Java, CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(use-package cc-mode
  :straight t
  :defer t
  :config
  (defun clang-format-save-hook ()
    "Run clang-format on save when in c or c++ mode when the variable
     'clang-forma-on-save' is set. Put the following into a .dir-locals.el file
     in your project to use this:

     ((nil . ((eval . (setq clang-format-on-save t)))))"
    (interactive)
    (when (and (boundp 'clang-format-on-save)
               (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode)))
      (clang-format-buffer)))
  (add-hook 'before-save-hook 'clang-format-save-hook)
  ;;
  ;; Add the following to a .dir-locals.el file in your C/C++ project:
  ;;
  ;; ((nil . ((eval . (add-hook 'after-save-hook 'export-compile-commands-foo)))))
  ;;
  ;; This will regenerate the `compile_commands.json' file after each save.
  ;;
  ;; (defvar company-backends (delete 'company-semantic company-backends))
  ;; (define-key c-mode-map [(tab)] 'company-complete)
  ;; (define-key c++-mode-map [(tab)] 'company-complete)
  )

;; Clang format
;; http://clang.llvm.org/docs/ClangFormat.html
;; http://clang.llvm.org/docs/ClangFormatStyleOptions.html
(use-package clang-format :straight t :defer t)

;; major-mode for editing CMake sources
;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode :defer t :straight t)

;; ;; Auto-completion for C/C++ headers using Company
;; ;; https://github.com/randomphrase/company-c-headers
;; (use-package company-c-headers
;;   :straight t
;;   :defer t
;;   :init
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode)
;;                   (add-to-list 'company-backends 'company-c-headers)))))

;; ;; company-mode completion back-end for irony-mode
;; ;; https://github.com/Sarcasm/company-irony
;; (use-package company-irony
;;   :straight t
;;   :defer t
;;   :init
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode)
;;                 (progn
;;                   (add-to-list 'company-backends 'company-irony)
;;                   (irony-mode))))))


;; ;; A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.
;; ;; http://www.rtags.net / https://github.com/Andersbakken/rtags
;; (use-package company-rtags
;;   :straight t
;;   :defer t
;;   :config
;;   (progn
;;     (setq-default rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq-default rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)))


;; ;; C, C++ and Objective-C support for Flycheck, using Irony Mode
;; ;; https://github.com/Sarcasm/flycheck-irony
;; (use-package flycheck-irony
;;   :straight t
;;   :config
;;   (eval-after-load 'flycheck
;;     '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; ;; A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs based on clang.
;; ;; http://www.rtags.net / https://github.com/Andersbakken/rtags
;; (use-package flycheck-rtags
;;   :straight t
;;   :defer t
;;   :config
;;   (progn
;;     ;; ensure that we use only rtags checking
;;     ;; https://github.com/Andersbakken/rtags#optional-1
;;     (defun setup-flycheck-rtags ()
;;       (flycheck-select-checker 'rtags)
;;       (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;       (setq-local flycheck-check-syntax-automatically nil)
;;       (rtags-set-periodic-reparse-timeout 2.0))  ;; Run flycheck 2 seconds after being idle.
;;     (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;     (add-hook 'c++-mode-hook #'setup-flycheck-rtags)))

;; ;; Shows an inline arguments hint for the C/C++ function at point
;; ;; https://github.com/abo-abo/function-args
;; (use-package function-args
;;   :straight t
;;   :config
;;   (fa-config-default)
;;   :bind
;;   (:map c-mode-map
;;         ("M-o" . fa-show))
;;   (:map c++-mode-map
;;         ("M-o" . fa-show)))

;; ;; A C/C++ minor mode for Emacs powered by libclang
;; ;; https://github.com/Sarcasm/irony-mode
;; (use-package irony
;;   :straight t
;;   :defer t
;;   :diminish abbrev-mode irony-mode
;;   :config
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
;;     (define-key irony-mode-map [remap complete-symbol] 'counsel-irony))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; ;; irony-mode support for eldoc-mode
;; ;; https://github.com/ikirill/irony-eldoc
;; (use-package irony-eldoc :defer t :straight t)

;;; c-cpp.el ends here
