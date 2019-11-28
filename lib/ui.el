;;; ui.el --- User interface changes.
;;; Commentary:
;; User interface changes.
;;; Code:

(unless (getenv "EMACS_NO_DIFF_HL") ;; Don't use diff-hl.
  ;; diff-hl - highlight changes/diffs
  ;; https://github.com/dgutov/diff-hl
  (use-package diff-hl
    :ensure t
    :config
    (global-diff-hl-mode)))

(unless (getenv "EMACS_NO_FIREPLACE") ;; Don't allow Emacs to be a warm cozy fireplace.
;;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
  (use-package fireplace
    :ensure t
    :init (defvar fireplace-mode-map)
    :bind (:map fireplace-mode-map
                ("d" . fireplace-down)
                ("s" . fireplace-toggle-smoke)
                ("u" . fireplace-up))))

(unless (getenv "EMACS_NO_STATUS_EMOJI") ;; Don't use emoji for Flycheck's status.
  ;; Flycheck Status Emoji
  ;; https://github.com/liblit/flycheck-status-emoji
  (use-package flycheck-status-emoji
    :ensure t
    :config
    (flycheck-status-emoji-mode)))

(unless (getenv "EMACS_NO_SMART_MODE_LINE") ;; Don't use smart-mode-line.
  ;; A powerful and beautiful mode-line for Emacs.
  ;; https://github.com/Malabarba/smart-mode-line
  ;; TODO: Possibly add support for extra themes (e.g. powerline and solarized)
  (let ((h/sml/themes
         #s(hash-table
            size 3
            test equal
            data
            ("dark" dark
             "light" light
             "respectful" respectful)))
        (h/sml/default-theme "dark"))

    (use-package smart-mode-line
      :ensure t
      :config
      (setq
       sml/shorten-directory t
       sml/theme
       (gethash (or (getenv "EMACS_USE_SML_THEME") h/sml/default-theme)
                          h/sml/themes nil))
      (sml/setup))))

(unless (getenv "EMACS_NO_THEME") ;; Don't load a theme.
  ;; Color Theme for emacs based on material design colors
  ;; https://github.com/cpaulik/emacs-material-theme
  ;; TODO: How to use darker grey vs blue colors?
  ;; Also, allow for using different themes;
  ;; from a list or perhaps an arbitrary one.
  (use-package material-theme
    :ensure t
    :config (load-theme 'material t)))

(if (getenv "EMACS_LINUM")
    ;; http://is.gd/Mw5KiS
    (global-linum-mode t))

(defvar h-font-height)
(if (getenv "RETINA_DISPLAY")
    (setq h-font-height 170)
  (setq h-font-height 100))

;; TODO: Support different fonts
(unless (getenv "EMACS_NO_HACK_FONT")
  ;; Hack - http://sourcefoundry.org/hack/
  (if (or (file-exists-p (concat (getenv "HOME") "/.fonts/Hack-Regular.ttf"))
          (file-exists-p "/usr/share/fonts/TTF/Hack-Regular.ttf"))
      (set-face-attribute 'default nil
                          :family "Hack"
                          :height h-font-height
                          :weight 'normal)))

(unless (getenv "EMACS_NO_NOTO_EMOJI")
  ;; I don't work on macOS much anymore.. but if i do:
  ;; (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") (selected-frame) 'prepend)
  ;; Emoji font
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") (selected-frame) 'prepend))

;; TODO: Make the below things toggle-able

(display-time-mode)

;; https://www.emacswiki.org/emacs/HighlightCurrentLine
(global-hl-line-mode t)

;; No toolbar, please.
(tool-bar-mode -1)

;; Or menu bar...
(menu-bar-mode -1)

;; Or scroll bar.
(scroll-bar-mode -1)

;; Maximize Emacs when it's opened
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; ui.el ends here
