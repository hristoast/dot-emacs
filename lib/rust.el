;;; rust.el -- for rust functionality

;;; Commentary:
;; Require this from init.el to enable Rust functionality.
;; TODO: this is out of date and needs to be revised.
;;; Code:
;;  Company integration for racer
;; https://github.com/emacs-pe/company-racer
(use-package company-racer :ensure t :ensure t)

;; https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust :ensure t :ensure t)

;; rust-mode: https://github.com/rust-lang/rust-mode
;; and emacs-racer: https://github.com/racer-rust/emacs-racer
(use-package racer
  :ensure t
  :init
  (defvar racer-cmd (concat (getenv "HOME") "/racer/target/release/racer"))
  (defvar racer-rust-src-path (concat (getenv "HOME") "/rust/src"))
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(provide 'rust)
;;; rust.el ends here
