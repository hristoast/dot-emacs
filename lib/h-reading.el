;;; h-reading.el --- Various reading tools/etc
;;; Commentary:
;; Stuff for reading stuff inside Emacs.
;;; Code:

(use-package nov
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;; h-reading.el ends here
