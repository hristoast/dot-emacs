;;; h-company.el --- Packages related to company mode.
;;; Commentary:
;; Packages related to company mode.
;;; Code:

;; Modular in-buffer completion framework for Emacs
;; http://company-mode.github.io/
(use-package company
  :straight t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (setq
   company-echo-delay 0
   company-idle-delay 0.0
   company-minimum-prefix-length 1
   company-tooltip-align-annotations t
   company-tooltip-limit 20)
  ;; Default colors are awful - borrowed these from gocode (thanks!):
  ;; https://github.com/nsf/gocode/tree/master/emacs-company#color-customization
  ;; See: M-x list-colors-display
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Colors.html
  (set-face-attribute
   'company-preview nil :foreground "black" :underline t)
  (set-face-attribute
   'company-preview-common nil :inherit 'company-preview)
  (set-face-attribute
   'company-tooltip nil :background "gray44" :foreground "black")
  (set-face-attribute
   'company-tooltip-selection nil :background "LavenderBlush4" :foreground "DarkOliveGreen2")
  (set-face-attribute
   'company-tooltip-common nil :foreground "OliveDrab3" :weight 'bold)
  (set-face-attribute
   'company-tooltip-common-selection nil :foreground "black" :weight 'bold))

(unless (getenv "EMACS_NO_COMPANY_ICONS")
  (use-package company-box
    :diminish company-box-mode
    :straight t
    :hook (company-mode . company-box-mode)))

;;; h-company.el ends here
