;;; fennel.el --- Support for fennel
;;; Commentary:
;; Enable support for fennel https://fennel-lang.org/
;;; Code:

;; https://gitlab.com/technomancy/fennel-mode/-/blob/686e4d28a8abeb1fa05cb21e14c4f0cc12217d63/Readme.md
(use-package fennel-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

;;; fennel.el ends here
