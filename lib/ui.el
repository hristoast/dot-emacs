;;; ui.el --- User interface changes.
;;; Commentary:
;; User interface changes.
;;; Code:

(unless (getenv "EMACS_NO_ALERT")
  ;; https://github.com/jwiegley/alert
  ;; A Growl-like alerts notifier for Emacs
  (use-package alert
    :straight t
    :config
    (let ((hristoast-alert-styles
           #s(hash-table
              size 8
              test equal
              data
              ("growl" growl
               "libnotify" libnotify
               "log" log
               "message" message
               "mode-line" mode-line
               "notifications" notifications
               "osx-notifier" osx-notifier
               "x11" x11)))
          (hristoast-alert-default-style "libnotify"))
      (setq-default alert-default-style (gethash (or (getenv "EMACS_ALERT_STYLE") hristoast-alert-default-style)
                                                 hristoast-alert-styles nil)))))

(unless (getenv "EMACS_NO_DIFF_HL") ;; Don't use diff-hl.
  ;; diff-hl - highlight changes/diffs
  ;; https://github.com/dgutov/diff-hl
  (use-package diff-hl
    :straight t
    :config
    (global-diff-hl-mode)))

(unless (getenv "EMACS_NO_DASHBOARD")
  (use-package page-break-lines :straight t :defer t)

  ;; An extensible emacs startup screen showing you what’s most important.
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  (use-package dashboard
    :straight t
    :config
    ;; (page-break-lines-mode)
    (dashboard-setup-startup-hook)
    ;; Allow banner customization through an environment variable
    (let ((hristoast-dashboard-banners
           #s(hash-table
              size 5
              test equal
              data
              ("official" official
               "logo" logo
               "1" 1
               "2" 2
               "3" 3)))
          (hristoast-dashboard-default-banner "logo"))
      (setq-default
       ;; For the page break lines
       dashboard-page-separator "\n\f\n"
       dashboard-banner-logo-title (or (getenv "EMACS_DASHBOARD_TITLE")
                                       "Welcome to Hristos' Emacs")
       dashboard-footer-messages '("Check out: https://mousikofidi.info/"
                                   "Check out: https://openmw.org/"
                                   "Check out: https://soupault.app/"
                                   "Check out: https://sr.ht/"
                                   "Don't forget to smile!"
                                   "Hristos is proud of you!"
                                   "Keep excited and hack on!"
                                   "Pssst, try: M-x fireplace"
                                   "Smile at a stranger today."
                                   "Read about available tweaks here: https://man.sr.ht/~hristoast/dot-emacs/config.md#tweaks"
                                   "Set these environment variables for a light theme: EMACS_MATERIAL_THEME=light EMACS_SML_THEME=light"
                                   "Set these environment variables for no extra themes: EMACS_NO_SMART_MODE_LINE=t EMACS_NO_STATUS_EMOJII=t EMACS_NO_THEME=t"
                                   "Set this environment variable for the dark blue material theme: EMACS_MATERIAL_THEME_BLUE=t"
                                   "Set this environment variable to change the default org file: EMACS_DEFAULT_ORG_FILE=/path/to/file.org")
       dashboard-items '((recents  . 20)
                         (bookmarks . 20)
                         (agenda . 20))
       dashboard-set-file-icons t
       dashboard-set-heading-icons t
       dashboard-startup-banner (gethash (or (getenv "EMACS_DASHBOARD_BANNER")
                                             hristoast-dashboard-default-banner)
                                         hristoast-dashboard-banners nil))
      ;; Don't setup the dashboard when Emacs is started with files as command-line arguments.
      ;; https://github.com/emacs-dashboard/emacs-dashboard/issues/115#issuecomment-478167057
      (if (< (length command-line-args) 2)
          (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))))))

(unless (getenv "EMACS_NO_ALL_THE_ICONS")
  (progn
    ;; A utility package to collect various Icon Fonts and propertize them within Emacs.
    ;; https://github.com/domtronn/all-the-icons.el
    (use-package all-the-icons :straight t)
    (use-package all-the-icons-dired
      :straight t
      :hook (dired-mode . all-the-icons-dired-mode))))

(unless (getenv "EMACS_NO_FIREPLACE") ;; Don't allow Emacs to be a warm cozy fireplace.
  ;; Warm cozy fireplace -- https://github.com/johanvts/emacs-fireplace
  (use-package fireplace
    :straight t
    :init (defvar fireplace-mode-map)
    :bind (:map fireplace-mode-map
                ("d" . fireplace-down)
                ("s" . fireplace-toggle-smoke)
                ("u" . fireplace-up))))

(unless (getenv "EMACS_NO_STATUS_EMOJI") ;; Don't use emoji for Flycheck's status.
  ;; Flycheck Status Emoji
  ;; https://github.com/liblit/flycheck-status-emoji
  (use-package flycheck-status-emoji
    :straight t
    :config
    (flycheck-status-emoji-mode)))

(or (getenv "EMACS_NO_SMART_MODE_LINE")
    (getenv "EMACS_NO_THEME")
    ;; A powerful and beautiful mode-line for Emacs.
    ;; https://github.com/Malabarba/smart-mode-line
    ;; TODO: Possibly add support for extra themes (e.g. powerline and solarized)
    (let ((hristoast-sml-themes
           #s(hash-table
              size 3
              test equal
              data
              ("dark" dark
               "light" light
               "respectful" respectful)))
          (hristoast-sml-default-theme "dark"))

      (use-package smart-mode-line
        :straight t
        :config
        (setq
         sml/shorten-directory t
         sml/theme
         (gethash (or (getenv "EMACS_SML_THEME") hristoast-sml-default-theme)
                  hristoast-sml-themes nil))

        (if (getenv "EMACS_TRUST_SML_THEMES")
            ;; This isn't totally cool but here's an option for it anyways:
            ;; https://github.com/Malabarba/smart-mode-line/blob/f53e380c9aadcba42ed2d4ed6ebc508b5e006519/README.org#installation-issues-faq
            (setq sml-no-confirm-load-theme t))

        (sml/setup))))

(unless (getenv "EMACS_NO_THEME") ;; Don't load a theme.
  ;; Color Theme for emacs based on material design colors
  ;; https://github.com/cpaulik/emacs-material-theme
  ;; But use my fork that always selects grey colors over darkblue.
  (use-package material-theme
    :straight (material-theme
               :type git
               :host nil
               :repo "https://git.sr.ht/~hristoast/emacs-material-theme")
    :config
    (if (getenv "EMACS_TRUST_THEMES")
        (setq hristoast-trust-themes t)
      (setq hristoast-trust-themes nil))

    (let ((hristoast-material-themes #s(hash-table
                                size 8
                                test equal
                                data
                                ("dark" material
                                 "light" material-light)))
          (hristoast-material-default "dark"))
      (setq-default hristoast-material-theme
                    (gethash (or (getenv "EMACS_MATERIAL_THEME") hristoast-material-default)
                             hristoast-material-themes nil)))

    (load-theme hristoast-material-theme hristoast-trust-themes)))

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
