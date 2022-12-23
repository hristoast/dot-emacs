;;; h-slime.el --- Load slime packages
;;; Commentary:
;; Packages related to slime.
;;; Code:

;; Superior LISP Interaction Mode for Emacs
;; https://common-lisp.net/project/slime/
;; Cool keybindings to remember:
;; C-c C-c Invoke slime-compile-defun
;; C-c C-l Load current buffer with slime-load-file
;; C-c C-k Compile and load current buffer
;; C-c C-q Invoke slime-close-parens-at-point
(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy)
        slime-net-coding-system 'utf-8-unix))

;;; h-slime.el ends here
