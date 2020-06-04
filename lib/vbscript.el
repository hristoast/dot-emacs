;;; vbscript.el --- Load visual basic stuff
;;; Commentary:
;; Packages related to visual basic.
;;; Code:


;; https://github.com/jwiegley/use-package/blob/d2640fec376a8458a669e7526e63e5870d875118/README.md#extending-the-load-path
(eval-and-compile
  (defun vbmode-site-load-path ()
    (concat user-emacs-directory "lib/")))

(when (file-exists-p (concat user-emacs-directory "lib/visual-basic-mode.el"))
  (use-package visual-basic-mode
    :defer t
    :load-path (lambda () (list (vbmode-site-load-path)))))

;;; vbscript.el ends here
