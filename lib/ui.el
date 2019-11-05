;;; ui.el --- User interface changes.
;;; Commentary:
;; User interface changes.
;;; Code:

(unless (getenv "EMACS_NO_DIFF_HL") ;; Don't use diff-hl.
  ;; diff-hl - highlight changes/diffs
  ;; https://github.com/dgutov/diff-hl
  (use-package diff-hl
    :config
    (global-diff-hl-mode)))

(unless (getenv "EMACS_NO_FIREPLACE") ;; Don't allow Emacs to be a warm cozy fireplace.
;;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
  (use-package fireplace
    :init (defvar fireplace-mode-map)
    :bind (:map fireplace-mode-map
                ("d" . fireplace-down)
                ("s" . fireplace-toggle-smoke)
                ("u" . fireplace-up))))

(unless (getenv "EMACS_NO_STATUS_EMOJII") ;; Don't use emojii for Flycheck's status.
  ;; Flycheck Status Emoji
  ;; https://github.com/liblit/flycheck-status-emoji
  (use-package flycheck-status-emoji
    :config
    (flycheck-status-emoji-mode)))

(unless (getenv "EMACS_NO_SMART_MODE_LINE") ;; Don't use smart-mode-line.
  ;; A powerful and beautiful mode-line for Emacs.
  ;; https://github.com/Malabarba/smart-mode-line
  (use-package smart-mode-line
    :config
    (setq
     sml/shorten-directory t
     sml/theme 'respectful)
    (sml/setup)))

(unless (getenv "EMACS_NO_THEME") ;; Don't load a theme.
  ;; Color Theme for emacs based on material design colors
  ;; https://github.com/cpaulik/emacs-material-theme
  ;; TODO: How to use darker grey vs blue colors?
  (use-package material-theme
    :config (load-theme 'material t)))

;;; ui.el ends here
